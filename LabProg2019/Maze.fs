(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Manuel Nardin @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx


let rnd = System.Random()
type CharInfo with
    static member wall = pixel.create(Config.filled_pixel_char, Color.DarkGray)
    static member background = pixel.create(Config.filled_pixel_char, Color.DarkBlue)
    static member player = pixel.create(char('\178'),Color.Red)
    static member internal path = pixel.filled Color.Green

// TASK 1: implement the maze type
type maze (width, height) =      

    member val walls = Array2D.init width height (fun x y -> not(x%2=1 && y%2=1))
    member val visited = Array2D.create width height false with get,set
    member val solution = Array2D.create width height (0,0) : (int*int)[,]
    
    
    member private this.isLegalPoint (x,y) =
        x > 0 && x < width-1 && y > 0 && y < height-1

    member private this.isNotLegalPoint(x,y) =
        not(this.isLegalPoint(x,y))

    member private this.neighbours (x,y) = 
        [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
        |> List.filter this.isLegalPoint
        |> List.sortBy (fun _ -> rnd.Next())             
    
    member private this.directNeighbours(x,y) =
        [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
        |>List.filter (fun (x,y) -> not this.walls.[x,y])
        |>List.filter this.isLegalPoint 

    member private this.rndPerimeterWall(x,y):int*int =
        [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
        |> List.filter this.isNotLegalPoint
        |> List.sortBy(fun x -> rnd.Next())
        |> List.head

    member private this.rndPerimeterCell() =
        [
            (rnd.Next(1, width-2), 1);//top cell
            (width-2, rnd.Next(1, height-2)); //right cell
            (rnd.Next(1, width-2), height-2);//bottom cell
            (1, rnd.Next(1, height-2)) //left cell
        ]
        |>List.sortBy(fun x -> rnd.Next())
        |>List.head
        
    ///Function that remove the wall between two cell.
    member private this.removeWallBetween ((x1,y1), (x2,y2)) =
        if x1 <> x2 then
            let wallPos = (x1+x2)/2
            this.walls.[wallPos, y1] <- false
            //this.solution.[wallPos , y1] <- x1, y1
            //if isLegalPoint(x2,y2) then
            //    this.solution.[x2, y2] <- wallPos , y1
            wallPos, y1
        else
            let wallPos = (y1+y2)/2
            this.walls.[x1, wallPos] <- false
        //this.solution.[x1, wallPos] <- x1,y1
        //if isLegalPoint(x2,y2) then
        //    this.solution.[x2,y2] <- x1 , wallPos
            x1, wallPos

    ///Create a random exit removing a random wall in perimeter
    member private this.createRandomExit() =        
        let randomCell = this.rndPerimeterCell()
        Log.msg "Creo un'uscita"
        let x,y as exitPoint = this.removeWallBetween((randomCell), this.rndPerimeterWall(randomCell))
        let nx,ny = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
                    |>List.filter this.isLegalPoint 
                    //|>List.exactlyOne
                    |>List.head

        if this.walls.[nx,ny] then
            this.walls.[x,y] <- true
            this.createRandomExit()
        else
            exitPoint                      

           
    ///Create an entrance removing one of the perimeter wall sorrounding the player
    member private this.createEntrance(x,y) = 
        this.removeWallBetween((x,y), this.rndPerimeterWall(x,y))

    member this.createMaze() =   
        ///Recursive function that visit any unvisited neighbours
        let rec visit (x,y as p) = 
          this.visited.[x,y] <- true

          for (nx,ny) as n in this.neighbours p do          
            if not this.visited.[nx,ny] then
              this.removeWallBetween(p,n)|> ignore
              visit n

              
        visit (1, 1)
        let entranceWall = this.createEntrance(1,1)
        let exitWall = this.createRandomExit()
        Log.msg "Entrance wall: %A. Exit wall: %A" entranceWall exitWall
        entranceWall, exitWall
          
    member this.drawMaze() =
        image(width,height,[|                
            for y in 0..height-1 do
               for x in 0..width-1 do                    
                   if this.walls.[x,y]  then
                        yield CharInfo.wall    
                   else yield CharInfo.background
                    |])

 
    
    member this.createSolution() =
        let rec solve (x,y as p) =
            this.visited.[x,y] <- true
            for(nx,ny) as n in this.directNeighbours p do
                if not this.visited.[nx,ny] then
                    this.solution.[nx,ny] <- x,y                        
                    solve n

        this.visited <- Array2D.init width height (fun _ _ -> false)  
      
        solve(1,1)


    member this.drawSolution(startPoint, endPoint) =
            this.createSolution()
    
            let solutionMatrix =
                let mutable p = endPoint
                [|
                yield p
                while not(p = startPoint) do 
                    p <- this.solution.[fst(p), snd(p)]
                    yield p     
                |] 
            
            image(width, height, [|
            for y in 0..height-1 do
                for x in 0..width-1 do          
                    if Array.contains (x,y) solutionMatrix then
                        if (x,y) = startPoint || (x,y) = endPoint then
                            yield pixel.filled(Color.Yellow)
                        else
                            yield CharInfo.path
                    else
                        yield CharInfo.empty
            |])





    



    