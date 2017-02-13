package scoreboard.classic;

public class Scoreboard {
	private TeamSelection selectedTeam = TeamSelection.NONE;

	private int teamAScore = 0;
	private int teamBScore = 0;

	public int scoreTeamA() {
		return teamAScore;
	}

	public int scoreTeamB() {
		return teamBScore;
	}

	public void selectTeamA() {
		selectedTeam = TeamSelection.A;
	}

	public boolean isTeamASelected() {
		return selectedTeam == TeamSelection.A;
	}

	public void selectTeamB() {
		selectedTeam = TeamSelection.B;
	}

	public boolean isTeamBSelected() {
		return selectedTeam == TeamSelection.B;
	}

	public void increment() {
		if (selectedTeam == TeamSelection.A)
			teamAScore++;
		if (selectedTeam == TeamSelection.B)
			teamBScore++;
	}

	public void decrement() {
		if (selectedTeam == TeamSelection.A)
			teamAScore--;
		if (selectedTeam == TeamSelection.B)
			teamBScore--;
	}

	public void resetScore() {
		setScore(0, 0);
		selectedTeam = TeamSelection.NONE;
	}

	public void setScore(int a, int b) {
		teamAScore = a;
		teamBScore = b;
	}
}
