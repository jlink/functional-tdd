package scoreboard.classic;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

public class ScoreboardAppTests {

	private Console console = mock(Console.class);
	private ScoreboardModel scoreboard = mock(ScoreboardModel.class);
	private ScoreboardApp app = new ScoreboardApp(console, scoreboard);

	@Nested
	class ScorePrinting {

		@Test
		void initialScoreIsTakenFromScoreboard() {
			setCurrentScoreboardScore(4, 5);
			when(console.readLine()).thenReturn("x");
			app.run();
			verify(console).println("004:005");
		}

		@Test
		void scoreIsPrintedIn000Format() {
			setCurrentScoreboardScore(99, 105);
			when(console.readLine()).thenReturn("x");
			app.run();
			verify(console).println("099:105");
		}
	}

	@Nested
	class Commands {
		@Test
		void commandASelectsTeamA() {
			when(console.readLine()).thenReturn("a", "x");
			when(scoreboard.isTeamASelected()).thenReturn(true);
			app.run();
			verify(scoreboard).selectTeamA();
			verify(console).println("Team A selected");
		}

		@Test
		void commandBSelectsTeamB() {
			when(console.readLine()).thenReturn("b", "x");
			when(scoreboard.isTeamBSelected()).thenReturn(true);
			app.run();
			verify(scoreboard).selectTeamB();
			verify(console).println("Team B selected");
		}

		@Test
		void commandPlusIncrementsScoreboardAndPrintsCurrentScore() {
			setCurrentScoreboardScore(11, 12);
			when(console.readLine()).thenReturn("+", "x");
			app.run();
			verify(scoreboard).increment();
			verify(console, times(2)).println("011:012");
		}

		@Test
		void commandMinusDecrementsScoreboardAndPrintsCurrentScore() {
			setCurrentScoreboardScore(12, 11);
			when(console.readLine()).thenReturn("-", "x");
			app.run();
			verify(scoreboard).decrement();
			verify(console, times(2)).println("012:011");
		}

		@Test
		void commandRResetsScoreOnScoreboardAndPrintsCurrentScore() {
			setCurrentScoreboardScore(0, 0);
			when(console.readLine()).thenReturn("r", "x");
			app.run();
			verify(scoreboard).resetScore();
			verify(console, times(2)).println("000:000");
		}

		@Test
		void commandsAreTrimmed() {
			when(console.readLine()).thenReturn("  b    ", "x");
			app.run();
			verify(scoreboard).selectTeamB();
		}

		@Test
		void commandsAreConvertedToLowercase() {
			when(console.readLine()).thenReturn("B", "x");
			app.run();
			verify(scoreboard).selectTeamB();
		}

		@Test
		void unknownCommandsAreIgnored() {
			when(console.readLine()).thenReturn("u", "x");
			app.run();
			verify(scoreboard).scoreTeamA();
			verify(scoreboard).scoreTeamB();
			verifyNoMoreInteractions(scoreboard);
		}

	}

	@Nested
	@Disabled
	class ForSlidesOnly {
		@Test
		void commandASelectsTeamA_usingSideEffect() {
			when(console.readLine()).thenReturn("a", "x");
			Scoreboard scoreboard = new Scoreboard();
			app = new ScoreboardApp(console, scoreboard);
			app.run();
			assertTrue(scoreboard.isTeamASelected());
		}

		@Test
		void commandASelectsTeamA_usingTestOnlyMethod() {
			Scoreboard scoreboard = new Scoreboard();
			app = new ScoreboardApp(console, scoreboard);
			app.executeCommand("a");
			assertTrue(scoreboard.isTeamASelected());
		}

		@Test
		void commandASelectsTeamA_usingCommandExecutor() {
			Scoreboard scoreboard = new Scoreboard();
			CommandExecutor executor = new CommandExecutor(scoreboard);
			executor.execute("a");
			assertTrue(scoreboard.isTeamASelected());
		}

	}

	private void setCurrentScoreboardScore(int teamA, int teamB) {
		when(scoreboard.scoreTeamA()).thenReturn(teamA);
		when(scoreboard.scoreTeamB()).thenReturn(teamB);
	}

}