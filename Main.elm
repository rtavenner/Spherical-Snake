import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL exposing (Mesh, Shader)
import Keyboard
import Matrices exposing (..)
import Time exposing (Time)
import AnimationFrame
import Set exposing (Set)
import Dict exposing (Dict)
import List.Extra as List

type alias Key = Int

type alias KeysModel = {keys : Set Key, model : Model}

type alias Model = Result {score : Int} GameData

type alias GameData = 
    { head : Mat4
    , tail : List Mat4
    , time : Float
    }

type Msg = 
    KeyDown Key
    | KeyUp Key
    | Tick Float

main : Program Never KeysModel Msg
main =
    Html.program
        { init = 
            { keys = Set.empty
            , model = Ok
                { head = Mat4.identity
                , tail = []
                , time = 0
                }
            } ! []
        , view = .model
            >> \m -> case m of
                Ok x -> view x
                Err {score} -> 
                    Html.div [style [("font-size","144px")]]
                        [Html.text ("Score: " ++ toString score)]
        , subscriptions = 
            (\{model} -> 
                case model of
                    Ok _ -> Sub.batch 
                        [ Keyboard.downs KeyDown
                        , Keyboard.ups KeyUp
                        , AnimationFrame.diffs (Tick << Time.inSeconds)] 
                    Err _ -> Sub.none)
        , update = update
        }


update : Msg -> KeysModel -> (KeysModel,Cmd Msg)
update msg m = 
    case msg of 
        KeyDown k -> { m | keys = Set.insert k m.keys } ! []
        KeyUp   k -> { m | keys = Set.remove k m.keys } ! []
        Tick dt ->
            case m.model of
                Ok data ->
                    { m 
                    | model = 
                        data
                        |> generalUpdates dt
                        |> Result.map 
                            (\data -> Set.foldr
                                (\k m -> 
                                    case Dict.get k keyUpdates of
                                        Nothing -> m
                                        Just f -> f dt m)
                                data
                                m.keys)
                    } ! []
                Err score -> m ! []

keyUpdates : Dict Key (Time -> GameData -> GameData)
keyUpdates = 
    Dict.fromList 
        [ (87, \dt m -> {m | head = Mat4.mul (turnUp     dt) m.head})
        , (83, \dt m -> {m | head = Mat4.mul (turnUp    -dt) m.head})
        , (65, \dt m -> {m | head = Mat4.mul (turnRight -dt) m.head})
        , (68, \dt m -> {m | head = Mat4.mul (turnRight  dt) m.head})
        ]

generalUpdates : Time -> GameData -> Model
generalUpdates dt 
    =  (\m -> { m | time = m.time - dt})
    >> (\m -> if m.time > 0 then m else { m | time = m.time + 0.2, tail = (::) m.head m.tail})
    >> (\m -> { m | head = Mat4.mul (moveForward (dt/2)) m.head})
    >> (\m -> 
        if List.any (tooClose m.head) <| List.dropWhile (tooClose m.head) m.tail
        then Err {score = List.length m.tail}
        else Ok m)

tooClose : Mat4 -> Mat4 -> Bool
tooClose a b =
    Vec4.distanceSquared
        (transform4 a (vec4 0 0 0 1))
        (transform4 b (vec4 0 0 0 1))
    < sphereRadiusSquared

--use one-side render mode
view : GameData -> Html Msg
view model =
    WebGL.toHtml
        [ width 1200
        , height 800
        , style [ ( "display", "block" ), ("border", "10px solid black"), ("background-color", "black") ]
        ]
        (List.map 
            (\pos ->
                WebGL.entity
                    vertexShader
                    fragmentShader
                    sphere
                    (uniforms pos model))
            (List.dropWhile (tooClose model.head) <| model.tail))

uniforms : Mat4 -> GameData -> Uniforms
uniforms objpos model =
    { eye = model.head
    , objectPosition = Mat4.transpose objpos
    , perspective = Mat4.makePerspective 80 (3/2) 0.01 1
    , shade = 0.8
    }

type alias Uniforms =
    { eye : Mat4
    , objectPosition : Mat4
    , perspective : Mat4
    , shade : Float
    }

type alias Vertex =
    { color : Vec3
    , position : Vec4 -- Should have x^2+y^2+z^2+w^2 == 1
    }

mkVertex : Vec3 -> Vec4 -> Vertex
mkVertex color position = Vertex color (Vec4.normalize position)


vertexShader : Shader Vertex Uniforms { vcolor : Vec4 }
vertexShader =
    [glsl|

        attribute vec4 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 eye;
        uniform mat4 objectPosition;
        varying vec4 vcolor;
        void main () {
            vec4 pos = eye * objectPosition * position;
            gl_Position = perspective * (pos + vec4(0.0,0.0,0.0,1.0));
            vec4 relpos = pos - vec4(0.0,0.0,0.0,1.0);
            float dist2 = relpos.x * relpos.x + relpos.y * relpos.y + relpos.z * relpos.z + relpos.w * relpos.w;
            vcolor = pos.w > 0.0 ? vec4((1.0 - (dist2 / 4.0)) * color, 1.0) : vec4(0.0,0.0,0.0,0.0); 
            // If the object is on the far side of the hypersphere, I don't want to show the face. This is supposed to fight flickering. It's not working.
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec4 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec4 vcolor;
        void main () {
            gl_FragColor = vec4(clamp(shade * vcolor.xyz, 0.1, 0.9), vcolor.w);
        }

    |]

phi : Float
phi = (sqrt 5 + 1) / 2

sphereRadiusSquared : Float
sphereRadiusSquared = Vec4.distanceSquared (vec4 0 0 0 1) (Vec4.normalize (vec4 0 phi 1 10))

-- Actually an icosahedron, but close enough.
sphere : Mesh Vertex
sphere =
    WebGL.indexedTriangles
        [ (mkVertex (vec3 1 1 1) (vec4 0  phi  1 10))
        , (mkVertex (vec3 1 1 1) (vec4 0  phi -1 10))
        , (mkVertex (vec3 1 1 1) (vec4 0 -phi -1 10))
        , (mkVertex (vec3 1 1 1) (vec4 0 -phi  1 10))
        
        , (mkVertex (vec3 1 1 1) (vec4  1 0  phi 10))
        , (mkVertex (vec3 1 1 1) (vec4 -1 0  phi 10))
        , (mkVertex (vec3 1 1 1) (vec4 -1 0 -phi 10))
        , (mkVertex (vec3 1 1 1) (vec4  1 0 -phi 10))
        
        , (mkVertex (vec3 1 1 1) (vec4  phi  1 0 10))
        , (mkVertex (vec3 1 1 1) (vec4  phi -1 0 10))
        , (mkVertex (vec3 1 1 1) (vec4 -phi -1 0 10))
        , (mkVertex (vec3 1 1 1) (vec4 -phi  1 0 10))
        ]
        [ ( 0, 1, 8)
        , ( 0, 1,11)
        , ( 2, 3, 9)
        , ( 2, 3,10)
        , ( 4, 5, 0)
        , ( 4, 5, 3)
        , ( 6, 7, 1)
        , ( 6, 7, 2)
        , ( 8, 9, 4)
        , ( 8, 9, 7)
        , (10,11, 5)
        , (10,11, 6)
        , ( 0, 4, 8)
        , ( 0, 5,11)
        , ( 3, 4, 9)
        , ( 3, 5,10)
        , ( 1, 7, 8)
        , ( 1, 6,11)
        , ( 2, 7, 9)
        , ( 2, 6,10)
        ]
