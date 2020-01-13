(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* MazeController.fs: MazeController
* (C) 2019 Manuel Nardin @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.MazeController

open System
open System.Diagnostics
open Gfx
open Engine
open Maze
open External


type MazeState = {
    maze: maze
    mazeSpr: sprite
    playerSpr: sprite
    elapsedTime : Stopwatch
    informationBar: sprite
    }

type SolutionState = {
    solution : sprite
}

type Player = {
    name : String
    score : int
    mazeState : MazeState    
    }



    
type MazeControl(w, h) =
    let mazeUpdate (key : ConsoleKeyInfo Option) (screen : wronly_raster) (st) =        
        let elapsedTimeStr = String.Concat(["Elapsed Time:"; (st.elapsedTime.Elapsed).ToString(@"mm\:ss") ])
        
        let exitCode = match key with
                       |Some key -> let dx, dy as nextMove=      
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

                       |None -> st,false

        st.informationBar.draw_text(elapsedTimeStr,0,0, Color.Red)
        exitCode   
        
    let solutionUpdate(key : ConsoleKeyInfo Option)(screen: wronly_raster)(st) =
        st, match key with
            |Some key -> key.KeyChar = 'q'
            |None -> false

    let mazeEngine = new engine(w,h)

    member this.NewGame(playerName) =
        mazeEngine.show_fps <- false
                
        let maze = maze(w,h-1)
        maze.createMaze()
        let mazeImg = maze.drawMaze()
        Log.msg "Create maze IMG"
        let mazeSpr = mazeEngine.create_and_register_sprite(mazeImg,0,0,0)
        Log.msg "Create maze Spr"    
        let playerSpr = mazeEngine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player),fst(maze.entry),snd(maze.entry),2)
        Log.msg "Create maze player"
        let informationBar = mazeEngine.create_and_register_sprite(image(w,1),0,h-1,0)
        
        let mazeState = {
            maze = maze
            mazeSpr = mazeSpr
            playerSpr = playerSpr
            elapsedTime = new Stopwatch()
            informationBar = informationBar
            }               
        
        let player = {
            name = playerName
            score = 0
            mazeState = mazeState
            }
                     
        Log.msg "Create player: %s" player.name
        mazeState.elapsedTime.Start()
        mazeEngine.loop mazeUpdate mazeState
        mazeState.elapsedTime.Stop()
        mazeEngine.remove_all_sprite()
        player

    member this.Solve(player: Player) =        
        let entry, exit = player.mazeState.maze.entry, player.mazeState.maze.exit
        let solutionImg= player.mazeState.maze.drawSolution(entry,exit)
        mazeEngine.create_and_register_sprite(player.mazeState.mazeSpr, 0,0,0) |> ignore
        let solutionSpr = mazeEngine.create_and_register_sprite(solutionImg,0,0,1)
        let solutionState = {
            solution = solutionSpr            
            }

        mazeEngine.loop solutionUpdate solutionState
        mazeEngine.remove_all_sprite()
        

    member this.Resume(player : Player) =
        let mazeSpr = mazeEngine.create_and_register_sprite(player.mazeState.mazeSpr, 0,0,0)
        let playerSpr = mazeEngine.create_and_register_sprite(image.rectangle(1,1, CharInfo.player), int(player.mazeState.playerSpr.x), int(player.mazeState.playerSpr.y), 1)
        let informationBar = mazeEngine.create_and_register_sprite(player.mazeState.informationBar,0,h-1,0)
        let mazeState = {
            maze = player.mazeState.maze
            mazeSpr = mazeSpr
            playerSpr = playerSpr
            elapsedTime = player.mazeState.elapsedTime
            informationBar = informationBar
        }
        mazeState.elapsedTime.Start()
        mazeEngine.loop mazeUpdate mazeState
        mazeState.elapsedTime.Stop()
        player.mazeState.playerSpr.x <- playerSpr.x
        player.mazeState.playerSpr.y <- playerSpr.y
        
        mazeEngine.remove_all_sprite()





    
