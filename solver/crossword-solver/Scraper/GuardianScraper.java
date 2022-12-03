import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.System.exit;

/**
 * Headless benchmarking class for testing the application.
 */
public class GuardianScraper {

    private static final String guardianUrl = "https://www.theguardian.com/crosswords/cryptic/";
    private static final String everymanUrl = "https://www.theguardian.com/crosswords/everyman/";

    public static void main(String[] args) {
        if (args.length < 4) {
            System.err.println("Not enough arguments. [guardian/everyman] [start#] [end#] [filename]");
            return;
        }
        String filePath = args[3];
        String crossword = args[0];
        String url;
        boolean isEveryman;
        if (crossword.equals("guardian")) {
            url = guardianUrl;
            isEveryman = false;
        } else if (crossword.equals("everyman")) {
            url = everymanUrl;
            isEveryman = true;
        } else {
            System.err.println("Invalid crossword");
            return;
        }
        int startNo = Integer.parseInt(args[1]);
        int endNo = Integer.parseInt(args[2]);
        if (endNo < startNo) {
            return;
        }
        try {
            Document doc = Jsoup.connect(url).get();
            Elements ele = doc.getElementsByClass("js-crossword");
            String data = ele.attr("data-crossword-data");
            //System.out.println(data);
            JsonParser jsonParser = new JsonParser();
            JsonObject json = jsonParser.parse(data).getAsJsonObject();
            JsonArray entries = json.getAsJsonArray("entries");
            for (JsonElement entry : entries) {
                JsonObject entryObj = entry.getAsJsonObject();
                String idString = entryObj.get("id").getAsString();
                ClueID id = ClueID.fromString(idString);
                String solution = entryObj.get("solution").getAsString();
                System.out.println(id.toString() + " " + solution);
                solutions.put(id, solution);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return solutions;
    }
}

