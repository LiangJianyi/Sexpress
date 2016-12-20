using System;
namespace ArithmeticSexp {
	public class Evaluator {
		private string [ ] tokens;

		public Evaluator ( string [ ] tokens ) {
			this.tokens = tokens;
		}

		/// <summary>
		/// 求值
		/// </summary>
		public double? Evaluting ( ) {
			Func<int, Node, Node> eval = null;
			eval = ( index, node ) => {
				/// <summary>
				/// 创建复合节点
				/// </summary>
				if ( this.tokens [ index ].Equals ( "(" ) ) {
					if ( index == 0 ) {
						node = new Node ( );
					}
					else {
						if ( node.LeftNode == null ) {
							node.LeftNode = new Node ( );
							eval ( index + 1, node.LeftNode );
							this.tokens = this.Reduction ( this.tokens, node.LeftNode.Value, index );
						}
						else {
							node.RightNode = new Node ( );
							eval ( index + 1, node.RightNode );
							this.tokens = this.Reduction ( this.tokens, node.RightNode.Value, index );
						}
					}
				}
				/// <summary>
				/// 指派运算符
				/// </summary>
				else if ( "+-*/%".Contains ( this.tokens [ index ] ) ) {
					node.Ops = this.tokens [ index ] [ 0 ];
				}
				/// <summary>
				/// 创建原子节点
				/// </summary>
				else if ( "0123456789".Contains ( this.tokens [ index ] [ 0 ].ToString ( ) ) ) {
					if ( node.LeftNode == null ) {
						node.LeftNode = new Node ( Convert.ToDouble ( this.tokens [ index ] ) );
					}
					else if ( node.RightNode == null ) {
						node.RightNode = new Node ( Convert.ToDouble ( this.tokens [ index ] ) );
					}
					//return eval ( index + 1, node );
				}
				/// <summary>
				/// 遇上了闭括号就返回当前节点
				/// </summary>
				else {
					return node;
				}
				return eval ( index + 1, node );
			};
			Node root = null;
			return eval ( 0, root ).Value;
		}

		/// <summary>
		/// 归约
		/// </summary>
		/// <param name="tokens">要归约的数组</param>
		/// <param name="num">归约节点的值</param>
		/// <param name="index">归约位置</param>
		private Func<string [ ], double?, int, string [ ]> Reduction = ( tokens, value, location ) => {
			string [ ] temp = new string [ tokens.Length - 4 ];
			int ignore = 0;
			for ( int token_index = 0, temp_index = 0; token_index < tokens.Length; token_index++ ) {
				if ( token_index < location ) {
					temp [ temp_index ] = tokens [ token_index ];
					temp_index++;
				}
				else if ( token_index == location ) {
					temp [ temp_index ] = value.ToString ( );
					temp_index++;
				}
				else {
					ignore += 1;
					if ( ignore > 4 ) {
						temp [ temp_index ] = tokens [ token_index ];
						temp_index++;
					}
				}
			}
			return temp;
		};
	}
}
