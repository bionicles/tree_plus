// csharp_test.cs
public interface IExcelTemplate
{
    void LoadTemplate(string templateFilePath);
    void LoadData(Dictionary<string, string> data);
    void ModifyCell(string cellName, string value);
    void SaveToFile(string filePath);
}

public interface IGreet
{
    void Greet();
}

public enum WeekDays
{
    Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday
}

public delegate void DisplayMessage(string message);

public struct Address
{
    public string city;
    public string state;
}

public static class HelperFunctions
{
    public static void PrintMessage(string message);
    public static int AddNumbers(int a, int b);
    private static float CalculateAverage(float[] numbers);
}

namespace HelloWorldApp {
    class Person : IGreet {
        public Person(string name, int age);
        public void Greet();
    }

    class HelloWorld {
        private static DisplayMessage displayDelegate;
        static void Main(string[] args);
    }
}

namespace TemplateToExcelServer.Template
{
    public interface ITemplateObject
    {
        bool ContentArrayAdded { get; }
        int? ContentArrayLength { get; }
        bool ContentValidForLoad { get; }
        DataTable DataTable { get; }
        string[] Format { get; }
        List<string> FormatList { get; }
        ReadOnlyMemory<byte> NameOfReport { get; }
        int? NumberOfRowsToAdd { get; }
        bool ProcessTemplate { get; }
        ReadOnlyMemory<byte> SheetName { get; }

        string[,] GetContent();
        string[] GetContentArray();
        string[] GetFormat();
        int? GetFormatLength();
        TemplateObject SetContent(string[,] Content);
        TemplateObject SetContentArray(string[] value);
        TemplateObject SetFormat(string[] Header);
        TemplateObject SetNameOfReport(ReadOnlyMemory<byte> ReportName);
        TemplateObject SetSheetName(ReadOnlyMemory<byte> SheetName);
    }
}

public class BankAccount(string accountID, string owner)
{
    public string AccountID { get; } = accountID;
    public string Owner { get; } = owner;

    public override string ToString() => $"Account ID: {AccountID}, Owner: {Owner}";
}

var IncrementBy = (int source, int increment = 1) => source + increment;

Func<int, int, int> add = (x, y) => x + y;

var filtered = list.Where(item => item.IsActive);

button.Click += (sender, args) => MessageBox.Show("Button clicked!");

public Func<int, int> GetMultiplier(int factor)
{
    return x => x * factor;
}