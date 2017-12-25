extern crate reqwest;
extern crate regex;

use std::io::Read;
use regex::Regex;

fn main() {
    let location = std::env::args().skip(1).next().expect("Usage: weather <location>");
    let weather_api_url = format!("http://api.openweathermap.org/data/2.5/weather?q={}&units=metric&mode=json&appid=886705b4c1182eb1c69f28eb8c520e20", location);
    let mut res = reqwest::get(&weather_api_url).expect("Request to google failed");
    let mut text = String::new();
    res.read_to_string(&mut text).expect("Failed to read response");

    let temp = Regex::new(r#""temp":([^,]*)"#).unwrap().captures(&text)
        .and_then(|x| x.get(1))
        .unwrap().as_str();
    let temp: f32 = temp.parse().expect("Failed to read date");
    let temp: f32 = temp.round();

    let conditions = vec![
        ("", Regex::new(r#""id":2\d{2}"#).unwrap()), // Thunderstorm
        ("", Regex::new(r#""id":6\d{2}"#).unwrap()), // Snow
        ("", Regex::new(r#""id":5\d{2}"#).unwrap()), // Rain
        ("", Regex::new(r#""id":3\d{2}"#).unwrap()), // Drizzle
        ("", Regex::new(r#""id":7\d{2}"#).unwrap()), // Atmospheric
        ("", Regex::new(r#""id":8\d{2}"#).unwrap()), // Clouds
    ];

    let mut icon = conditions.iter().fold(
        ("", None),
        |acc, x| match acc {
            (_, None) => (x.0, x.1.find(&text)),
            found => found
        }
    ).0;

    if icon == "" {
        icon = "";
    }

    println!("{} {} °C", icon, temp);
}
