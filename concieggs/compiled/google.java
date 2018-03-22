import java.io.*;
import java.net.*;

class Google {

    public static void main(String[] args) throws Exception {

        String key="AIzaSyAv3LmGV0U4uN6XWGJUI6GlUDi5BSxUXsM";
        String qry = "concieggs";
        if (args.length > 0) qry = URLEncoder.encode(String.join("  ", args));
        URL url = new URL(
                "https://www.googleapis.com/customsearch/v1?key="+key+ "&cx=012472299030023714099:r_r3vfplqiu&q="+ qry + "&alt=json");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Accept", "application/json");
        BufferedReader br = new BufferedReader(new InputStreamReader(
                    (conn.getInputStream())));

        String output;
        while ((output = br.readLine()) != null) {

            if(output.contains("\"link\": \"")){                
                String link=output.substring(output.indexOf("\"link\": \"")+("\"link\": \"").length(), output.indexOf("\","));
                System.out.println("Debug: " + qry);
                System.out.println("Klik bare på " + link);
                break;
            }     
        }
        conn.disconnect();                              
    }
}
