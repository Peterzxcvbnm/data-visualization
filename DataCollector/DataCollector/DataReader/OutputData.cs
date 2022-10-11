using Camille.RiotGames.Enums;

namespace DataReader;

public class OutputData
{
    public long gameDuration { get; set; }
    public int assists { get; set; }
    public string championName { get; set; }
    public int deaths { get; set; }
    public int goldEarned { get; set; }
    public int kills { get; set; }
    public Team teamId { get; set; }
    public string teamPosition { get; set; }
    public int totalDamageDealt { get; set; }
    public int visionScore { get; set; }
    public int wardsKilled { get; set; }
    public int wardsPlaced { get; set; }
    public bool win { get; set; }
    public long gameId { get; set; }
}