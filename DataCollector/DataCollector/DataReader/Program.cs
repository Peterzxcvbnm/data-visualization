// See https://aka.ms/new-console-template for more information

using System.Text;
using System.Text.Json;
using DataReader;

const string outputDir = "./output";
const string input = outputDir + "/data.json";
const string outputFile = outputDir + "/selected-data.csv";

Directory.CreateDirectory(outputDir);

var data = File.Exists(input) ? 
    JsonSerializer.Deserialize<DataStructure>(await File.ReadAllTextAsync(input)) : new DataStructure();

var selectVariables = data.Matches.Values.SelectMany(m =>
{
    return m.Info.Participants.Select(p => new OutputData()
        {
            gameId = m.Info.GameId,
            assists = p.Assists,
            championName = p.ChampionName,
            deaths = p.Deaths,
            kills = p.Kills,
            gameDuration = m.Info.GameDuration,
            goldEarned = p.GoldEarned,
            teamId = p.TeamId,
            win = p.Win,
            teamPosition = p.TeamPosition,
            totalDamageDealt = p.TotalDamageDealt,
            visionScore = p.VisionScore,
            wardsKilled = p.WardsKilled,
            wardsPlaced = p.WardsPlaced,
            totalMinionsKilled = p.TotalMinionsKilled,
        })
        .ToList();
});

var sb = new StringBuilder();

var properties = typeof(OutputData).GetProperties();
foreach(var prop in properties)
{
    sb.Append(prop.Name);
    sb.Append(',');
}
sb.AppendLine();

foreach(var d in selectVariables)
{
    foreach(var prop in properties)
    {
        sb.Append(prop.GetValue(d));
        sb.Append(',');
    }
    sb.AppendLine();
}

await File.WriteAllTextAsync(outputFile, sb.ToString());