package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
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
	// parse all input .json files
	names, paths := listAllMessageFiles("./input")
	for i, path := range paths {
		fmt.Println("Working on: ", path)
		parseMessageFile(path, names[i])
	}

	// combine output into single file
	_, paths = listAllMessageFiles("./output")
	var combinedPages []Page
	for _, path := range paths {
		// read file
		dat, err := ioutil.ReadFile(path)
		check(err)

		// read the json into an empty thread
		var pages []Page
		json.Unmarshal(dat, &pages)
		combinedPages = append(combinedPages, pages...)
	}

	// write to file
	fmt.Println("Total messages: ", len(combinedPages))
	out, _ := json.MarshalIndent(combinedPages, "", "  ")
	outPath := "./combined.json"
	ioutil.WriteFile(outPath, out, 0644)
}

// scans a directory for .json files
// outputs lists of file names and paths
func listAllMessageFiles(startDir string) ([]string, []string) {
	var fileNames []string
	var filePaths []string
	err := filepath.Walk(startDir,
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if strings.HasSuffix(info.Name(), ".json") {
				fileNames = append(fileNames, info.Name())
				filePaths = append(filePaths, path)
			}
			return nil
		})
	if err != nil {
		log.Println(err)
	}
	return fileNames, filePaths
}

// takes a messages file (path and name) ending in .json
// writes content to ./output/ directory
func parseMessageFile(filePath string, fileName string) {
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
	outPath := "./output/" + fileName + u1.String() + ".json"
	err = ioutil.WriteFile(outPath, out, 0644)
}
