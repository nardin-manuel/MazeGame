(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Gfx
open System.Text
open Engine

let rnd = System.Random()
type CharInfo with
    static member v_wall = pixel.create (Config.vertical_wall_pixel_char, Color.White)
    static member h_wall = pixel.create (Config.horizontal_wall_pixel_char, Color.White)
    static member wall = pixel.create(Config.filled_pixel_char, Color.White)
    static member joinWall = pixel.create(Config.join_wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black

// TASK 1: implement the maze type



type state = {
    maze : sprite
    player: sprite
}

type maze (width, height) =        
    //let choose (xs:_ list) = xs.[rnd_int 0 xs.Length]
    let horizWalls = Array2D.create width height true
        // (x,y) -> is there a wall between (x,y) and (x,y+1)?
    let vertWalls = Array2D.create width height  true
    let visited = Array2D.create width height false
    // (x,y) -> is there a wall between (x,y) and (x+1,y)?
    let solution = Array2D.create width height (0,0)


    member this.createMaze =
        let createGrid =
            for y in 0..height-1 do
                for x in 0..width-1 do
                    if x%2 = 1 (*|| y = width-1*) then
                        horizWalls.[x,y] <- false
                    if y%2 = 1 (*|| x = height-1*) then
                        vertWalls.[x,y]<- false
        let isLegalPoint (x,y) =
          x > 0 && x < width-1 && y > 0 && y < height-1

        let neighbours (x,y) = 
          [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
          |> List.filter isLegalPoint
          |> List.sortBy (fun x -> rnd.Next())
 
        let removeWallBetween (x1,y1) (x2,y2) =
          if x1 <> x2 then
            horizWalls.[(min x1 x2)+1, y1] <- false            
          else
            vertWalls.[x1, (min y1 y2)+1] <- false
            

        let rec visit (x,y as p) = 
          visited.[x,y] <- true //salvo che quella cella la ho visitata 
          for (nx,ny) as n in neighbours p do          
            if not visited.[nx,ny] then //se quel vicino non lo ho visitato
              visited.[x,y] <- true
              solution.[nx,ny] <- x,y
              removeWallBetween p n //tolgo il muro
              visit n //lo visito
 
        createGrid
        //visit (rnd_int 1 width, rnd_int 1 height)
        visit (1, 1)
        //horizWalls, vertWalls
        vertWalls,horizWalls

    member this.drawMaze(horizWalls : bool[,], vertWalls : bool[,]) =
        image(width,height, [|
            for y in 0..height-1 do
                //yield CharInfo.wall
                for x in 0..width-1 do                    
                    //if horizWalls.[x,y] && vertWalls.[x,y] then
                    //    yield CharInfo.joinWall
                    if horizWalls.[x,y]  then
                        yield CharInfo.h_wall          
                    else if vertWalls.[x,y] then
                        yield CharInfo.v_wall
                    else yield CharInfo.empty
                    |])

let main()=
    let w = 61
    let h = 61
    let engine = new engine (w+1,h+1)
    engine.show_fps <- false
    let offset_w = 0
    let offset_h = 0
    let maze = maze(w, h)    
    let mazeImg = maze.drawMaze(maze.createMaze)
    let backgroud = image.rectangle(w+2,h+2,pixel.create('*', Color.Red))
    let mazeSpr = engine.create_and_register_sprite(mazeImg,offset_w,offset_h,0)
    let player = engine.create_and_register_sprite(image.rectangle(1,1, pixel.create(char('*'), Color.Red)),offset_w+1,offset_h+1,0)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let dx, dy =
            match key.KeyChar with
            |'w' -> 0.,-1.
            |'a'-> -1.,0.
            |'s'-> 0.,1.
            |'d'-> 1.,0.
            |_ -> 0.,0.

        //screen.
        if not (st.player.checkCollissionWith(dx, dy, mazeSpr)) then
            st.player.move_by(dx, dy)
        st, key.KeyChar = 'q'
    
    //engine.create_and_register_sprite(backgroud,offset_w,offset_h,0) |> ignore

    let st0 = { 
        maze = mazeSpr
        player = player
        }

    engine.loop_on_key my_update st0