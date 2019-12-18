module LabProg2019.MazeController

open System
open Gfx
open Engine
open Maze
open System.Collections

type Player = {
    name : String
    score : int
    mazeNumber : int
    }

type mazeState = {
    maze: maze
    mazeSpr: sprite
    playerSpr: sprite
    }
    
type mazeControl(w, h) =
    let mazeUpdate (key : ConsoleKeyInfo) (screen : wronly_raster) (st) =
        let dx, dy as nextMove=
            match key.KeyChar with
            |'w' -> 0.,-1.
            |'a'-> -1.,0.
            |'s'-> 0.,1.
            |'d'-> 1.,0.
            |_ -> 0.,0.   

        if not (st.playerSpr.checkCollissionWith(nextMove, st.mazeSpr, CharInfo.wall)) then
            st.playerSpr.move_by(dx, dy)
            Log.msg "Player position: x:%f,y:%f" st.playerSpr.x st.playerSpr.y
        st, key.KeyChar = 'q'
    
    let solutionUpdate(key : ConsoleKeyInfo Option)(screen: wronly_raster)(st) =
        st, key.Value.KeyChar = 'q'

    member private this.mazeEngine = new engine(w,h)
    member private this.players = new ArrayList()
    member private this.mazes = new ArrayList()

    member this.newGame() =
        let maze = maze(w,h)
        let mazeSpr = this.mazeEngine.create_and_register_sprite(maze.drawMaze(),0,0,0)    
        let playerSpr = this.mazeEngine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player),1,1,2)
        let mazeState = {
            maze = maze
            mazeSpr = mazeSpr
            playerSpr = playerSpr
            }   
            
        let player = {
            name = "Gianni"
            score = 0
            mazeNumber = this.mazes.Add(mazeState)
            }
        this.players.Add(player) |> ignore     

        this.mazeEngine.loop_on_key mazeUpdate mazeState

    member this.solve(maze : maze) =
        let solutionImg= maze.drawSolution((1,1),(31,31))
        let solutionSpr = this.mazeEngine.create_and_register_sprite(solutionImg,0,0,1)
        let solutionState = {
            solution = solutionSpr            
            }

        this.mazeEngine.loop solutionUpdate solutionState
        

    member this.resume(player : Player) =        
        this.mazeEngine.loop_on_key mazeUpdate 
    
