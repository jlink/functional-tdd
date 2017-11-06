package scoreboard.classic;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

import net.jqwik.api.*;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

public class ScoreboardAppProperties implements Console {

	private ScoreboardModel scoreboard = new Scoreboard();
	private ScoreboardApp app = new ScoreboardApp(this, scoreboard);
	private int lineCount = 0;


	@Property(reporting = ReportingMode.GENERATED)
	void manyValidCommandsCanBeHandled(@ForAll("commands") List<String> commands) {
		lineCount = 0; // Should not be necessary
		for (String command : commands) {
			app.executeCommand(command);
		}
		Assertions.assertThat(lineCount).isEqualTo(commands.size());
	}

	@Provide
	Arbitrary<String> commands() {
		return Arbitraries.of("a", "b", "+", "-", "r");
	}

	@Override
	public void println(String line) {
		lineCount++;
	}

	@Override
	public String readLine() {
		return null;
	}
}