﻿using Camille.RiotGames.MatchV5;


public class DataStructure
{
    public Queue<string> SummonerIds { get; set; } = new();
    public HashSet<string> UsedSummonerIds { get; set; } = new();
    public Dictionary<string, Match> Matches { get; set; } = new();
}