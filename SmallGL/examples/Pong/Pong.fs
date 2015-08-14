module Pong

open System.Drawing
open OpenTK
open OpenTK.Input

type rect = RectangleF
type vec2 = Vector2

type Input = {
   mutable keyboardState : KeyboardState
}

type Ball = { mutable r : rect ; mutable vel : vec2 }

type Pong = { wall : rect ; mutable p1 : rect ; mutable p2 : rect ; ball : Ball ; startSpeed : float32 }

type GameData = { mutable input : Input; mutable pong : Pong }

let createGameData (wallWidth, wallHeight, paddleWidth, paddleHeight, ballSize, startSpeed) = {
      input = { keyboardState = KeyboardState () }
      pong =
         {
            wall = rect (0.f, 0.f, wallWidth, wallHeight)
            p1 = rect (wallWidth / 50.f, (wallHeight - paddleHeight) / 2.f, paddleWidth, paddleHeight)
            p2 = rect (wallWidth - (paddleWidth + wallWidth / 50.f),
                              (wallHeight - paddleHeight) / 2.f, paddleWidth, paddleHeight)
            ball = 
               { r = rect (wallWidth / 2.f, wallHeight / 2.f, ballSize, ballSize) ;
                  vel = vec2 (startSpeed, startSpeed) }
            startSpeed = startSpeed
         }
   }

let updateInput input =
   input.keyboardState <- OpenTK.Input.Keyboard.GetState ()

let movePaddle (ks : KeyboardState, paddle : rect byref, wall : rect, up : Key, down : Key, paddleSpeed)  =
   if ks.IsKeyDown up then
      paddle.Y <- paddle.Y - paddleSpeed
      if paddle.Top < wall.Top then paddle.Y <- wall.Top
   if ks.IsKeyDown down then
      paddle.Y <- paddle.Y + paddleSpeed
      if paddle.Bottom > wall.Bottom then paddle.Y <- wall.Bottom - paddle.Height

/// Contains pong update logic
let updatePong (pong, input) =
   // Move ball
   let b = pong.ball
   b.r.Offset (b.vel.X, b.vel.Y)

   // Move paddles
   let paddleSpeed = 5.f
   let ks = input.keyboardState
   movePaddle (ks, &pong.p1, pong.wall, Key.W, Key.S, 5.f)
   movePaddle (ks, &pong.p2, pong.wall, Key.Up, Key.Down, 5.f)

   let accelerationOnBounce = 0.2f

   let yChange (b : rect) (p : rect) =
      let yCentre (r : rect) = r.Top + r.Height / 2.f
      ((yCentre b) - (yCentre p)) / 10.f

   // TODO: The rectangle intersection check will break if the ball is going too
   // fast, which is easy enough to fix, but this is only a toy example.

   // Moving right
   if b.vel.X > 0.f then
      // Bounce off right paddle
      if b.r.IntersectsWith pong.p2 then
         b.r.Offset (pong.p2.Left - b.r.Right, 0.f)
         b.vel.X <- -b.vel.X - accelerationOnBounce
         b.vel.Y <- b.vel.Y + yChange b.r pong.p2
      // Test for contact with wall
      if b.r.Right > pong.wall.Right then
         b.r.Location <- PointF (pong.wall.Width / 2.f, pong.wall.Height / 2.f)
         b.vel <- vec2 -pong.startSpeed
   // Moving left
   if b.vel.X < 0.f then
      // Bounce off left paddle
      if b.r.IntersectsWith pong.p1 then
         b.r.Offset (pong.p1.Right - b.r.Left, 0.f)
         b.vel.X <- -b.vel.X + accelerationOnBounce
         b.vel.Y <- b.vel.Y + yChange b.r pong.p1
      // Test for contact with wall
      if b.r.Left < pong.wall.Left then
         b.r.Location <- PointF (pong.wall.Width / 2.f, pong.wall.Height / 2.f)
         b.vel <- vec2 pong.startSpeed
   // Bounce off bottom wall
   if b.vel.Y > 0.f then
      if b.r.Bottom > pong.wall.Bottom then
         b.r.Offset (pong.wall.Bottom - b.r.Bottom, 0.f)
         b.vel.Y <- -b.vel.Y
   // Bounce off top wall
   if b.vel.Y < 0.f then
      if b.r.Top < pong.wall.Top then
         b.r.Offset (pong.wall.Top - b.r.Top, 0.f)
         b.vel.Y <- -b.vel.Y   

/// Main game update method
let update data =
   updateInput data.input
   updatePong (data.pong, data.input)