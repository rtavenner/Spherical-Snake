module Matrices exposing (..)
import Math.Vector4 exposing (vec4,Vec4)
import Math.Matrix4 exposing (..)

fromTuples : ((Float,Float,Float,Float),(Float,Float,Float,Float),(Float,Float,Float,Float),(Float,Float,Float,Float)) -> Mat4
fromTuples ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = fromRecord
    { m11 = a, m21 = b, m31 = c, m41 = d 
    , m12 = e, m22 = f, m32 = g, m42 = h 
    , m13 = i, m23 = j, m33 = k, m43 = l 
    , m14 = m, m24 = n, m34 = o, m44 = p 
    }

moveForward : Float -> Mat4
moveForward theta =
    fromTuples
        ((1,0,        0,          0 )
        ,(0,1,        0,          0 )
        ,(0,0,cos theta,-(sin theta))
        ,(0,0,sin theta,  cos theta ))

moveRight : Float -> Mat4
moveRight theta =
    fromTuples
        ((   cos theta,0,0,sin theta)
        ,(           0,1,0,        0)
        ,(           0,0,1,        0)
        ,(-(sin theta),0,0,cos theta))

moveUp : Float -> Mat4
moveUp theta =
    fromTuples
        ((1,           0,0,        0)
        ,(0,   cos theta,0,sin theta)
        ,(0,           0,1,        0)
        ,(0,-(sin theta),0,cos theta))

turnRight : Float -> Mat4
turnRight theta =
    fromTuples
        ((cos theta,0,-(sin theta),0)
        ,(        0,1,          0 ,0)
        ,(sin theta,0,  cos theta ,0)
        ,(        0,0,          0 ,1))

turnUp : Float -> Mat4
turnUp theta =
    fromTuples
        ((1,        0,          0 ,0)
        ,(0,cos theta,-(sin theta),0)
        ,(0,sin theta,  cos theta ,0)
        ,(0,        0,          0 ,1))

spin : Float -> Mat4
spin theta =
    fromTuples
        ((cos theta,-(sin theta),0,0)
        ,(sin theta,  cos theta ,0,0)
        ,(        0,          0 ,1,0)
        ,(        0,          0 ,0,1))


transform4 : Mat4 -> Vec4 -> Vec4
transform4 m v =
    let {m11,m21,m31,m41,m12,m22,m32,m42,m13,m23,m33,m43,m14,m24,m34,m44} = toRecord m
        {x,y,z,w} = Math.Vector4.toRecord v
    in vec4 
        (m11*x+m21*y+m31*z+m41*w)
        (m12*x+m22*y+m32*z+m42*w)
        (m13*x+m23*y+m33*z+m43*w)
        (m14*x+m24*y+m34*z+m44*w)

