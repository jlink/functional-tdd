package scoreboard.classic;

public interface ScoreboardModel {
	int scoreTeamA();

	int scoreTeamB();

	void selectTeamA();

	boolean isTeamASelected();

	void selectTeamB();

	boolean isTeamBSelected();

	void increment();

	void decrement();

	void resetScore();
}
