import java.util.Calendar;
import java.util.GregorianCalendar;

public class Problem_19
{
	public static void main (String[] args)
	{
		Calendar calendar = new GregorianCalendar(1901, Calendar.JANUARY, 1);
		int answer = 0;
		while (calendar.get(Calendar.YEAR) != 2001)
		{
			calendar.add(Calendar.MONTH, 1);
			if (calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY)
				answer++;
		}
		System.out.println(answer);
	}
	static void print(Calendar calendar)
	{
		System.out.println(
			"YEAR: "+calendar.get(Calendar.YEAR)+"\n"+
			"MONTH: "+calendar.get(Calendar.MONTH)+"\n"+
			"DAY_OF_WEEK: "+calendar.get(Calendar.DAY_OF_WEEK));
	}
}