package scoreboard.classic;

public class Scoreboard implements ScoreboardModel {
	private TeamSelection selectedTeam = TeamSelection.NONE;

	private int teamAScore = 0;
	private int teamBScore = 0;

	public Scoreboard() {
		this(0, 0, TeamSelection.NONE);
	}

	public Scoreboard(int teamAScore, int teamBScore, TeamSelection selectedTeam) {
		this.teamAScore = teamAScore;
		this.teamBScore = teamBScore;
		this.selectedTeam = selectedTeam;
	}

	@Override
	public int scoreTeamA() {
		return teamAScore;
	}

	@Override
	public int scoreTeamB() {
		return teamBScore;
	}

	@Override
	public void selectTeamA() {
		selectedTeam = TeamSelection.TeamA;
	}

	@Override
	public boolean isTeamASelected() {
		return selectedTeam == TeamSelection.TeamA;
	}

	@Override
	public void selectTeamB() {
		selectedTeam = TeamSelection.TeamB;
	}

	@Override
	public boolean isTeamBSelected() {
		return selectedTeam == TeamSelection.TeamB;
	}

	@Override
	public void increment() {
		if (selectedTeam == TeamSelection.TeamA)
			teamAScore++;
		if (selectedTeam == TeamSelection.TeamB)
			teamBScore++;
	}

	@Override
	public void decrement() {
		if (selectedTeam == TeamSelection.TeamA)
			teamAScore = Math.max(teamAScore - 1, 0);
		if (selectedTeam == TeamSelection.TeamB)
			teamBScore = Math.max(teamBScore - 1, 0);
	}

	@Override
	public void resetScore() {
		setScore(0, 0);
		selectedTeam = TeamSelection.NONE;
	}

	public void setScore(int a, int b) {
		teamAScore = a;
		teamBScore = b;
	}

	@Override
	public String toString() {
		return String.format("Scoreboard (%s,%s) %s", teamAScore, teamBScore, selectedTeam);
	}
}
