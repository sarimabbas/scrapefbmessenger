package main

import (
	"encoding/json"
	"io/ioutil"
	"strings"

	"github.com/badoux/goscraper"
	uuid "github.com/satori/go.uuid"
	"mvdan.cc/xurls/v2"
)

// Message container for a facebook message
type Message struct {
	SenderName string `json:"sender_name"`
	Timestamp  uint   `json:"timestamp_ms"`
	Content    string `json:"content"`
}

// Page container for a parsed facebook message
type Page struct {
	// scraper fields
	Icon        string
	Name        string
	Title       string
	Description string
	Image       string
	URL         string

	// fb message
	SenderName string
	Timestamp  uint
	Message    string
}

// Thread container
type Thread struct {
	Messages []Message
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	parseMessageFile("./sample.json")
}

func parseMessageFile(filePath string) {
	// read file
	dat, err := ioutil.ReadFile(filePath)
	check(err)

	// read the json into an empty thread
	var thread Thread
	json.Unmarshal(dat, &thread)
	messages := thread.Messages

	// filter messages
	rxRelaxed := xurls.Relaxed()
	var pages []Page
	for _, m := range messages {
		url := rxRelaxed.FindString(m.Content)
		if url != "" {
			if !strings.HasPrefix(url, "http") {
				url = "http://" + url
			}

			f := Page{}
			// fb message content
			f.Message = m.Content
			f.SenderName = m.SenderName
			f.Timestamp = m.Timestamp
			// scrape stuff
			scrapeResult, err := goscraper.Scrape(url, 5)
			if err != nil {
				continue
			}
			f.Icon = scrapeResult.Preview.Icon
			f.Name = scrapeResult.Preview.Name
			f.Title = scrapeResult.Preview.Title
			f.Description = scrapeResult.Preview.Description
			f.URL = scrapeResult.Preview.Link
			if len(scrapeResult.Preview.Images) > 1 {
				f.Image = scrapeResult.Preview.Images[1]
			} else if len(scrapeResult.Preview.Images) > 0 {
				f.Image = scrapeResult.Preview.Images[0]
			}
			// append to pages
			pages = append(pages, f)
		}

	}

	// write to file
	out, err := json.MarshalIndent(pages, "", "  ")
	u1 := uuid.NewV4()
	outPath := "./output/" + filePath + u1.String() + ".json"
	err = ioutil.WriteFile(outPath, out, 0644)
}
