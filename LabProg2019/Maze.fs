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
    static member internal path = pixel.filled Color.Green

// TASK 1: implement the maze type



type state = {
    maze : sprite
    player: sprite
}

type maze (width, height) =        
    member val walls = Array2D.init width height (fun x y -> not(x%2=1 && y%2=1)) // same as -> if x%2=1 && y%2=1 then false else true
    member val visited = Array2D.create width height false
    member val solution = Array2D.create width height (0,0) : (int*int)[,]


    member this.createMaze() =   
        let isLegalPoint (x,y) =
          x > 0 && x < width-1 && y > 0 && y < height-1
        let isNotLegalPoint(x,y) =
            not(isLegalPoint(x,y))

        let neighbours (x,y) = 
          [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
          |> List.filter isLegalPoint
          |> List.sortBy (fun x -> rnd.Next())

        let perimeterWall(x,y):int*int =
            [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
            |> List.filter isNotLegalPoint
            |> List.sortBy(fun x -> rnd.Next())
            |> List.head

        let perimeterCells() =
            [(rnd.Next(1, width-1), 1);//top cell
             (width-1, rnd.Next(1, height-1)); //right cell
             (rnd.Next(1, width-1), height-1);//bottom cell
             (1, rnd.Next(1, height-1)) //left cell
            ]
            |>List.sortBy(fun x -> rnd.Next())
            |>List.head


        ///Function that remove the wall between two cell.
        let removeWallBetween ((x1,y1), (x2,y2)) =
          if x1 <> x2 then            
            this.walls.[(x1+x2)/2, y1] <- false
            this.solution.[(x1+x2)/2 , y1] <- x1, y1
            if isLegalPoint(x2,y2) then
                this.solution.[x2, y2] <- (x1+x2)/2 , y1
           else
            this.walls.[x1, (y1+y2)/2] <- false
            this.solution.[x1, (y1+y2)/2] <- x1,y1
            if isLegalPoint(x2,y2) then
                this.solution.[x2,y2] <- x1 , (y1+y2)/2

        ///Create a random exit removing a random wall in perimeter
        let createRandomExit = 
            let randomCell = perimeterCells()
            removeWallBetween(randomCell, perimeterWall(randomCell))

        ///Create an entrance removing one of the perimeter wall sorrounding the player
        let createEntrance(x,y) = 
            removeWallBetween((x,y), perimeterWall(x,y))

        ///Recursive function that visit any unvisited neighbours
        let rec visit (x,y as p) = 
          this.visited.[x,y] <- true
          for (nx,ny) as n in neighbours p do          
            if not this.visited.[nx,ny] then
              this.visited.[x,y] <- true
              removeWallBetween(p,n)
              visit n
          
        
        createEntrance(1,1)
        visit (1, 1)
        createRandomExit
        

    member this.drawMaze() =
        image(width,height,[|                 
            for y in 0..height-1 do
               for x in 0..width-1 do                    
                   if this.walls.[x,y]  then
                        yield CharInfo.wall    
                   else yield CharInfo.background
                    |])
    
    member this.solveMaze(startPoint, endPoint) =
        let createSolutionMatrix =
            let mutable p = endPoint
            [|
            yield p
            while not(p = startPoint) do 
                p <- this.solution.[fst(p), snd(p)]
                yield p     
            |]

        let solution = createSolutionMatrix            
        
        image(width, height, [|
        for y in 0..height-1 do
            for x in 0..width-1 do          
                if Array.contains (x,y) solution then
                    if (x,y) = startPoint || (x,y) = endPoint then
                        yield pixel.filled(Color.Yellow)
                    else
                        yield CharInfo.path
                else
                    yield CharInfo.empty
        |])

            
            
        


                

let main()=
    let w = 201
    let h = 61
    let engine = new engine (w,h)
    engine.show_fps <- false
    let offset_w = 0
    let offset_h = 0
    let maze = maze(w, h)
    maze.createMaze()
    let mazeImg = maze.drawMaze() 
    let solutionImg = maze.solveMaze((1,1) , (73,19))
    let mazeSpr = engine.create_and_register_sprite(mazeImg,offset_w,offset_h,0)
    let player = engine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player),offset_w+1,offset_h+1,2)
    let solutionSpr = engine.create_and_register_sprite(solutionImg,offset_w,offset_h,1)


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
            Log.msg "Player position: x:%f,y:%f" st.player.x st.player.y
        st, key.KeyChar = 'q'


    let st0 = { 
        maze = mazeSpr
        player = player
        }

    engine.loop_on_key my_update st0