namespace ArithmeticSexp {
	public class Tokenizer {
		private string [ ] tokens;
		private const char space = ' ';

		public Tokenizer ( string express ) {
			this.tokens = express.Split ( space );
		}

		public string [ ] GetTokens ( ) {
			return this.tokens;
		}
	}
}
