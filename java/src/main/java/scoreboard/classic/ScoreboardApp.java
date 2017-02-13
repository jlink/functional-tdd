package scoreboard.classic;

public class ScoreboardApp {
	private final Console console;
	private final Scoreboard scoreboard;

	public ScoreboardApp(Console console, Scoreboard scoreboard) {
		this.scoreboard = scoreboard;
		this.console = console;
	}

	public void run() {
		printCurrentScore();
		while(true) {
			String command = console.readLine().trim().toLowerCase();
			if (command.equals("x"))
				break;
			executeCommand(command);
		}
	}

	void executeCommand(String command) {
		if (command.equals("a")) {
			scoreboard.selectTeamA();
			printTeamSelection();
		}
		if (command.equals("b")) {
			scoreboard.selectTeamB();
			printTeamSelection();
		}
		if (command.equals("+")) {
			scoreboard.increment();
			printCurrentScore();
		}
		if (command.equals("-")) {
			scoreboard.decrement();
			printCurrentScore();
		}
		if (command.equals("c")) {
			scoreboard.resetScore();
			printCurrentScore();
		}
	}

	private void printTeamSelection() {
		if (scoreboard.isTeamASelected())
			console.println("Team A selected");
		if (scoreboard.isTeamBSelected())
			console.println("Team B selected");
	}

	private void printCurrentScore() {
		console.println(formatScore(scoreboard.scoreTeamA(), scoreboard.scoreTeamB()));
	}

	private String formatScore(int scoreTeamA, int scoreTeamB) {
		return String.format("%03d:%03d", scoreTeamA, scoreTeamB);
	}

}
