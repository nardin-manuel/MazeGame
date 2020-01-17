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

type maze (width, height) =      
    
    member val walls = Array2D.init width height (fun x y -> not(x%2=1 && y%2=1))
    member val visited = Array2D.create width height false with get,set
    member val solution = Array2D.create width height (0,0) : (int*int)[,]
    member val entry = 1,1 with get, set
    member val exit = 1,1 with get, set
    //Explicit stack to use in backtracking
    member val stack = new System.Collections.Generic.Stack<int*int>()
      
    //Return the cell that is inside the perimeter
    member private this.isLegalPoint (x,y) =
        x > 0 && x < width-1 && y > 0 && y < height-1

    member private this.isWall(x,y) = 
        this.walls.[x,y]

    member private this.isVisited(x,y) =
        this.visited.[x,y]
    
    //Return neighbors cell through wall
    member private this.neighbours (x,y) = 
        [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
        |> List.filter this.isLegalPoint
        |> List.sortBy (fun _ -> rnd.Next())             
    
    //Return neighbors cell including cell that was wall if is not a wall anymore
    member private this.directNeighbours(x,y) =
        [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
        |>List.filter (fun (x,y) -> x >= 0 && x < width && y >= 0 && y < height)
        |>List.filter (not << this.isWall)

    //Return a random cell that is a perimeter wall of the cell. 
    member private this.rndPerimeterWall(x,y):int*int =
        [(x-2,y);(x+2,y);(x,y-2);(x,y+2)]
        |> List.filter (not << this.isLegalPoint)
        |> List.sortBy(fun _ -> rnd.Next())
        |> List.head

    //Return a random cell that is a perimeter cell
    member private this.rndPerimeterCell() =
        [(rnd.Next(1, width-2), 1);//top cell
         (width-2, rnd.Next(1, height-2)); //right cell
         (rnd.Next(1, width-2), height-2);//bottom cell
         (1, rnd.Next(1, height-2))] //left cell        
        |>List.sortBy(fun _ -> rnd.Next())
        |>List.head
        
    //Return a random cell that is not blocked by a wall
    member private this.rndPathCell() =
        let x = rnd.Next(0, width-1)
        let y = rnd.Next(0, height-1)

        if not this.walls.[x, y] then
            x,y
        else this.rndPathCell()

    ///Function that remove the wall between two cell.
    member private this.removeWallBetween ((x1,y1), (x2,y2)) =
        if x1 <> x2 then
            let wallPos = (x1+x2)/2
            this.walls.[wallPos, y1] <- false
            wallPos, y1
        else
            let wallPos = (y1+y2)/2
            this.walls.[x1, wallPos] <- false
            x1, wallPos

    ///Create a random exit removing a random wall in perimeter
    member private this.createRandomExit() =        
        let randomCell = this.rndPerimeterCell()
        Log.msg "Creo un'uscita"
        let x,y as exitPoint = this.removeWallBetween((randomCell), this.rndPerimeterWall(randomCell))
        let nx,ny = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
                    |>List.filter this.isLegalPoint                     
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
        //let rec recursiveBacktracker (x,y as p) = 
        //  this.visited.[x,y] <- true
        //  for (nx,ny) as n in this.neighbours p do          
        //    if not this.visited.[nx,ny] then
        //      this.removeWallBetween(p,n)|> ignore
        //      recursiveBacktracker n
              
        let stackBacktracker(x,y as p) =
            this.stack.Push(p)
            this.visited.[x,y] <- true
            while this.stack.Count > 0 do
                let currentCell = this.stack.Pop()
                let neighbors = this.neighbours currentCell           
                match List.tryFind (not << this.isVisited) neighbors with
                |Some (nx, ny as neighbor) -> this.visited.[nx,ny] <- true
                                              this.removeWallBetween(currentCell, neighbor) |> ignore
                                              this.stack.Push(currentCell)
                                              this.stack.Push(neighbor)
                |None -> ()
                            
                    
        //recursiveBacktracker (1, 1)
        stackBacktracker(1,1)

        let entranceWall = this.createEntrance(1,1)
        let exitWall = this.createRandomExit()
        Log.msg "Entrance wall: %A. Exit wall: %A" entranceWall exitWall
        this.entry <- entranceWall
        this.exit <- exitWall
        
          
    member this.drawMaze() =
        image(width,height,[|                
            for y in 0..height-1 do
               for x in 0..width-1 do                    
                   if this.walls.[x,y]  then
                        yield CharInfo.wall    
                   else yield CharInfo.background
                    |])

 
    
    member this.createSolution() =
        //let rec solve (x,y as p) =
        //    this.visited.[x,y] <- true
        //    for(nx,ny) as n in this.directNeighbours p do
        //        if not this.visited.[nx,ny] then
        //            this.solution.[nx,ny] <- x,y                        
        //            solve n



        let stackSolve(x,y as p) = 
            this.visited.[x,y]<- true
            this.stack.Push(p)
            while this.stack.Count > 0 do
            let currentCell = this.stack.Pop()
            let neighbors = this.directNeighbours currentCell
            match List.tryFind (not << this.isVisited) neighbors with
            |Some (nx,ny as neighbor) -> this.stack.Push(currentCell)
                                         this.stack.Push(neighbor)
                                         this.visited.[nx,ny] <- true
                                         this.solution.[nx,ny] <- currentCell
            |None -> ()

        this.visited <- Array2D.init width height (fun _ _ -> false)
        this.stack.Clear()
        stackSolve(0,0)



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





    



    