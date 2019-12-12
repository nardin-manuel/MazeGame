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
    static member wall = pixel.create(Config.filled_pixel_char, Color.DarkGray)
    static member background = pixel.create(Config.filled_pixel_char, Color.DarkBlue)
    static member player = pixel.create(char('\178'),Color.Red)
    static member internal path = pixel.filled Color.Black

// TASK 1: implement the maze type



type state = {
    maze : sprite
    player: sprite
}

type maze (width, height) =        
    let walls = Array2D.init width height (fun x y -> not(x%2=1 && y%2=1)) // same as -> if x%2=1 && y%2=1 then false else true
    let visited = Array2D.create width height false
    let solution = Array2D.create width height (0,0)


    member this.createMaze =   
        let isLegalPoint (x,y) =
          x > 0 && x < width-1 && y > 0 && y < height-1

        let neighbours (x,y) = 
          [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
          |> List.filter isLegalPoint
          |> List.sortBy (fun x -> rnd.Next())
 
        let removeWallBetween (x1,y1) (x2,y2) =
          if x1 <> x2 then
            
            walls.[(x1+x2)/2, y1] <- false            
          else
            walls.[x1, (y1+y2)/2] <- false
            

        let rec visit (x,y as p) = 
          visited.[x,y] <- true //salvo che quella cella la ho visitata 
          for (nx,ny) as n in neighbours p do          
            if not visited.[nx,ny] then //se quel vicino non lo ho visitato
              visited.[x,y] <- true
              solution.[nx,ny] <- x,y
              removeWallBetween p n //tolgo il muro
              visit n //lo visito

        visit (1, 1)
        walls

    member this.drawMaze(horizWalls : bool[,]) =
        image(width,height,[|                 
            for y in 0..horizWalls.GetLength(1)-1 do
               for x in 0..horizWalls.GetLength(0)-1 do                    
                   if horizWalls.[x,y]  then
                        yield CharInfo.wall    
                   else yield CharInfo.background
                    |])
                

let main()=
    let w = 61
    let h = 21
    let engine = new engine (w,h)
    engine.show_fps <- false
    let offset_w = 0
    let offset_h = 0
    let maze = maze(w, h)    
    let mazeImg = maze.drawMaze(maze.createMaze)   
    let mazeSpr = engine.create_and_register_sprite(mazeImg,offset_w,offset_h,0)
    let player = engine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player),offset_w+1,offset_h+1,1)


    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let dx, dy as nextMove=
            match key.KeyChar with
            |'w' -> 0.,-1.
            |'a'-> -1.,0.
            |'s'-> 0.,1.
            |'d'-> 1.,0.
            |_ -> 0.,0.

        if not (st.player.checkCollissionWith(nextMove, mazeSpr, CharInfo.wall)) then
            st.player.move_by(dx, dy)
        st, key.KeyChar = 'q'


    let st0 = { 
        maze = mazeSpr
        player = player
        }

    engine.loop_on_key my_update st0