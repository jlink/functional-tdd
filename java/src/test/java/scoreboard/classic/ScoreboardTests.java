package scoreboard.classic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class ScoreboardTests {

	Scoreboard scoreboard = new Scoreboard();

	@Test
	void initialScoreIs000to000() {
		assertScore(0, 0);
	}

	private void assertScore(int expectedA, int expectedB) {
		assertEquals(expectedA, scoreboard.scoreTeamA(), "Team A Score");
		assertEquals(expectedB, scoreboard.scoreTeamB(), "Team B Score");
	}

	@Test
	void initiallyNoTeamIsSelected() {
		assertFalse(scoreboard.isTeamASelected());
		assertFalse(scoreboard.isTeamBSelected());
	}

	@Test
	void selectingTeamAMakesItSelected() {
		scoreboard.selectTeamA();
		assertTrue(scoreboard.isTeamASelected());
	}

	@Test
	void selectingTeamBMakesItSelected() {
		scoreboard.selectTeamB();
		assertTrue(scoreboard.isTeamBSelected());
	}

	@Test
	void lastSelectCallIsRelevant() {
		scoreboard.selectTeamB();
		scoreboard.selectTeamA();
		assertTrue(scoreboard.isTeamASelected());
		assertFalse(scoreboard.isTeamBSelected());
		scoreboard.selectTeamA();
		assertTrue(scoreboard.isTeamASelected());
		assertFalse(scoreboard.isTeamBSelected());
		scoreboard.selectTeamB();
		assertTrue(scoreboard.isTeamBSelected());
		assertFalse(scoreboard.isTeamASelected());
	}

	@Test
	void incrementIncrementsScoreOfSelectedTeam() {
		scoreboard.selectTeamA();
		scoreboard.increment();
		assertScore(1, 0);
		scoreboard.increment();
		assertScore(2, 0);
		scoreboard.selectTeamB();
		scoreboard.increment();
		assertScore(2, 1);
		scoreboard.increment();
		scoreboard.increment();
		scoreboard.increment();
		assertScore(2, 4);
	}

	@Test
	void decrementDecrementsScoreOfSelectedTeam() {
		scoreboard.setScore(10, 10);
		scoreboard.selectTeamA();
		scoreboard.decrement();
		assertScore(9, 10);
		scoreboard.selectTeamB();
		scoreboard.decrement();
		scoreboard.decrement();
		scoreboard.decrement();
		assertScore(9, 7);
	}

	@Test
	void whenNoTeamIsSelectedIncrementAndDecrementLeaveScoreAsIs() {
		scoreboard.setScore(10, 10);
		scoreboard.increment();
		assertScore(10, 10);
		scoreboard.decrement();
		assertScore(10, 10);
	}

	@Test
	void resetScoreSetsScoreTo0to0() {
		scoreboard.setScore(10, 10);
		scoreboard.resetScore();
		assertScore(0, 0);
	}

	@Test
	void noTeamSelectedAfterReset() {
		scoreboard.selectTeamA();
		scoreboard.resetScore();
		assertFalse(scoreboard.isTeamASelected());
		assertFalse(scoreboard.isTeamBSelected());
	}
}
