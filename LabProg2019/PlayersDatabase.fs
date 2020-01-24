(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* PlayersDatabase.fs: PlayersDatabase
* (C) 2019 Manuel Nardin @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.PlayersDatabase
open System.Collections.Generic
open System.Linq
open System
open Controller

type PlayersDatabase() =
    let playersList = List<Player>()
    
    member this.addPlayer(player: Player) =
        playersList.Add(player)

    member this.removePlayer(player: Player) = 
        playersList.Remove(player)

    member this.getPlayerFromName(playerName: String) =
        playersList.Find(fun player -> player.name = playerName)

    member this.playerExist(playerName:String) =        
        playersList.Exists(fun player -> player.name = playerName)      
            
    member this.getPlayersList() = 
        playersList

    member this.toList() =
        [
        for player in playersList do
            yield player.name
        ]

    member this.hasNotFinished() =
        //[
        //for player in playersList do
        //    if player.score = 0. then
        //        yield player
        //]
        playersList.Where(fun player -> player.score = 0.).ToList()
    
    member this.hasFinished() = 
        playersList.Where(fun player -> not(player.score = 0.)).ToList()
