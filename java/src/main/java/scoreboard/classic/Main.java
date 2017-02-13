package scoreboard.classic;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main {
	public static void main(String[] args) throws IOException {
		Console console = new SystemConsole();
		ScoreboardApp app = new ScoreboardApp(console, new Scoreboard());
		System.out.println("SCOREBOARD started.");
		app.run();
		System.out.println("SCOREBOARD stopped.");
	}

	private static class SystemConsole implements Console {

		private BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

		@Override
		public void println(String line) {
			System.out.println(line);
		}

		@Override
		public String readLine() {
			try {
				return in.readLine();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}
}
