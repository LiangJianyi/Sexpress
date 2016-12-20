namespace ArithmeticSexp {
	public class Node {
		private double? _value;
		internal char Ops;
		internal Node LeftNode;
		internal Node RightNode;
		internal double? Value
		{
			get
			{
				if (this._value == null) {
					double? result = null;
					switch (this.Ops) {
						case '+':
							result = this.LeftNode.Value + this.RightNode.Value;
							break;
						case '-':
							result = this.LeftNode.Value - this.RightNode.Value;
							break;
						case '*':
							result = this.LeftNode.Value * this.RightNode.Value;
							break;
						case '/':
							result = this.LeftNode.Value / this.RightNode.Value;
							break;
					}
					return result;
				}
				else {  //	如果 Node 实例非空，就证明当前实例为原子节点
					return this._value;
				}
			}
		}
		/// <summary>
		/// 此构造器实例化一个复合节点
		/// </summary>
		internal Node( ) { }
		/// <summary>
		/// 此构造器实例化一个原子节点
		/// </summary>
		/// <param name="value">Value.</param>
		internal Node( double value ) {
			this._value = value;
		}
	}
}
