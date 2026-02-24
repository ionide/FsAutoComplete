namespace InheritDocLib
{
    /// <summary>A class that provides fully documented members.</summary>
    public class DocumentedProvider
    {
        /// <summary>Gets the documented value.</summary>
        /// <returns>Always returns 42.</returns>
        public int GetValue() => 42;

        /// <summary>Gets a named result.</summary>
        /// <param name="name">The name to use.</param>
        /// <returns>A greeting string.</returns>
        public string GetNamed(string name) => $"Hello, {name}!";
    }

    /// <summary>A class whose members inherit documentation via inheritdoc cref.</summary>
    public class InheritDocConsumer
    {
        private readonly DocumentedProvider _inner = new DocumentedProvider();

        /// <inheritdoc cref="DocumentedProvider.GetValue"/>
        public int GetValue() => _inner.GetValue();

        /// <inheritdoc cref="DocumentedProvider.GetNamed"/>
        public string GetNamed(string name) => _inner.GetNamed(name);
    }
}
