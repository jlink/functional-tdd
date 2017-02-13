package scoreboard.classic;

public class CommandExecutor {
	private final Scoreboard scoreboard;

	public CommandExecutor(Scoreboard scoreboard) {
		this.scoreboard = scoreboard;
	}

	public void execute(String command) {
		if (command.equalsIgnoreCase("a"))
			scoreboard.selectTeamA();

	}
}
