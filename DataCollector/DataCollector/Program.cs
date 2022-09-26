// See https://aka.ms/new-console-template for more information

using System.Text.Json;
using Camille.Enums;
using Camille.RiotGames;
using DataCollector;

const string outputPath = "./output/data.json";

var riotApi = InitializeAPI(args);

var initialSummoner = await riotApi.SummonerV4().GetBySummonerNameAsync(PlatformRoute.EUW1, "Peterzxcvbnm");

var data = File.Exists(outputPath) ? 
    JsonSerializer.Deserialize<DataStructure>(await File.ReadAllTextAsync(outputPath)) : new DataStructure();

data.SummonerIds.Enqueue(initialSummoner.Puuid);
while (data.SummonerIds.TryDequeue(out var id))
{
    Console.WriteLine($"Working with {id}");
    var matchIds = await GetMatchIdsByPUUID(riotApi, id);

    Console.WriteLine("Getting matches");
    await GetMatches(matchIds, data, riotApi);

    data.UsedSummonerIds.Add(id);
}

await File.WriteAllTextAsync(outputPath, JsonSerializer.Serialize(data));
Console.WriteLine("Done writing to file");
Console.WriteLine("Press any button to exit...");
Console.Read();


RiotGamesApi InitializeAPI(string[] strings)
{
    string key;
    if (strings.Length == 0)
    {
        Console.WriteLine("Please input a riot API key:");
        key = Console.ReadLine();
    }
    else
    {
        key = strings[0];
    }

    Console.WriteLine($"Using API key: {key}");

    var riotGamesApi = RiotGamesApi.NewInstance(key);
    return riotGamesApi;
}

async Task<Queue<string>> GetMatchIdsByPUUID(RiotGamesApi riotApi1, string id)
{
    var queue = new Queue<string>();
    var index = 0;
    while (true)
    {
        var matches = await riotApi1.MatchV5().GetMatchIdsByPUUIDAsync(RegionalRoute.EUROPE,
            id,
            100,
            DateTimeOffset.Now.ToUnixTimeSeconds(),
            Queue.SUMMONERS_RIFT_5V5_RANKED_SOLO,
            DateTimeOffset.Now.AddDays(-7).ToUnixTimeSeconds(),
            index);
        foreach (var matchId in queue)
        {
            queue.Enqueue(matchId);
        }
        Console.WriteLine($"Gotten {matches.Length} match ids");

        if (matches.Length < 100)
        {
            break;
        }

        index += matches.Length;
    }
    Console.WriteLine($"Found a total of {queue.Count} matches for {id}");

    return queue;
}

async Task GetMatches(Queue<string> matchIds, DataStructure dataStructure, RiotGamesApi riotGamesApi1)
{
    while (matchIds.TryDequeue(out var matchid))
    {
        if (dataStructure.Matches.ContainsKey(matchid)) continue;
        
        var match = await riotGamesApi1.MatchV5().GetMatchAsync(RegionalRoute.EUROPE, matchid);
        dataStructure.Matches.Add(matchid, match);
        Console.WriteLine($"Got match: {matchid}");
    }
}