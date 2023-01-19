use std::{fs::File, io::Write, path::Path};
use zip::{write::FileOptions, ZipWriter};
use crate::projector::Rendered;

fn draw(target: &Path, canvas_size: (usize, usize), rendered: &Vec<Rendered>) {
    let mut content = String::new();

    content += &format!(
        r#"<?xml version="1.0" encoding="utf-8"?>
    <geogebra format="5.0" version="5.0.752.0" app="geometry" platform="w" id="ba72fcff-6ad1-4b3a-8e33-f8de335b0a37"  xsi:noNamespaceSchemaLocation="http://www.geogebra.org/apps/xsd/ggb.xsd" xmlns="" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" >
    <gui>
        <window width="1920" height="891" />
        <perspectives>
    <perspective id="tmp">
        <panes>
            <pane location="" divider="0.19739583333333333" orientation="1" />
        </panes>
        <views>
            <view id="4097" visible="false" inframe="false" stylebar="true" location="1,1,1,1" size="400" window="100,100,700,550" />
            <view id="512" toolbar="0 | 1 501 5 19 , 67 | 2 15 45 18 , 7 37 | 514 3 9 , 13 44 , 47 | 16 51 | 551 550 11 ,  20 22 21 23 , 55 56 57 , 12 | 69 | 510 511 , 512 513 | 533 531 , 534 532 , 522 523 , 537 536 , 535 , 538 | 521 520 | 36 , 38 49 560 | 571 30 29 570 31 33 | 17 | 540 40 41 42 , 27 28 35 , 6 , 502" visible="false" inframe="false" stylebar="false" location="1,1,1" size="500" window="100,100,600,400" />
            <view id="4" toolbar="0 || 2020 , 2021 , 2022 || 2001 , 2003 , 2002 , 2004 , 2005 || 2040 , 2041 , 2042 , 2044 , 2043" visible="false" inframe="false" stylebar="false" location="1,1" size="300" window="100,100,600,400" />
            <view id="8" toolbar="1001 | 1002 | 1003  || 1005 | 1004 || 1006 | 1007 | 1010 || 1008 | 1009 || 6" visible="false" inframe="false" stylebar="false" location="1,3" size="300" window="100,100,600,400" />
            <view id="1" visible="true" inframe="false" stylebar="false" location="1" size="1525" window="100,100,600,400" />
            <view id="2" visible="true" inframe="false" stylebar="false" location="3" size="379" tab="TOOLS" window="100,100,250,400" />
            <view id="16" visible="false" inframe="false" stylebar="false" location="1" size="300" window="50,50,500,500" />
            <view id="32" visible="false" inframe="false" stylebar="true" location="1" size="300" window="50,50,500,500" />
            <view id="64" toolbar="0" visible="false" inframe="false" stylebar="false" location="1" size="480" window="50,50,500,500" />
            <view id="128" visible="false" inframe="false" stylebar="false" location="1" size="480" window="50,50,500,500" />
            <view id="70" toolbar="0 || 2020 || 2021 || 2022" visible="false" inframe="false" stylebar="true" location="1" size="900" window="50,50,500,500" />
        </views>
        <toolbar show="true" items="0 77 73 62 | 1 501 67 , 5 19 , 72 75 76 | 2 15 45 , 18 65 , 7 37 | 4 3 8 9 , 13 44 , 58 , 47 | 16 51 64 , 70 | 10 34 53 11 , 24  20 22 , 21 23 | 55 56 57 , 12 | 36 46 , 38 49  50 , 71  14  68 | 30 29 54 32 31 33 | 25 17 26 60 52 61 | 40 41 42 , 27 28 35 , 6" position="1" help="false" />
        <input show="true" cmd="true" top="algebra" />
        <dockBar show="false" east="false" />
    </perspective>
        </perspectives>
        <labelingStyle  val="3"/>
        <font  size="16"/>
    </gui>
    <euclidianView>
        <viewNumber viewNo="1"/>
        <size  width="1525" height="891"/>
        <coordSystem xZero="762.5" yZero="445.5" scale="50" yscale="50.00000000000001"/>
        <evSettings axes="false" grid="false" gridIsBold="false" pointCapturing="3" rightAngleStyle="1" checkboxSize="26" gridType="3"/>
        <bgColor r="255" g="255" b="255"/>
        <axesColor r="37" g="37" b="37"/>
        <gridColor r="192" g="192" b="192"/>
        <lineStyle axes="1" grid="0"/>
        <axis id="0" show="false" label="" unitLabel="" tickStyle="1" showNumbers="true"/>
        <axis id="1" show="false" label="" unitLabel="" tickStyle="1" showNumbers="true"/>
    </euclidianView>
    <algebraView>
        <mode val="3"/>
    </algebraView>
    <kernel>
        <continuous val="false"/>
        <usePathAndRegionParameters val="true"/>
        <decimals val="1"/>
        <angleUnit val="degree"/>
        <algebraStyle val="1" spreadsheet="1"/>
        <coordStyle val="0"/>
    </kernel>
    <tableview min="0" max="0" step="0"/>
    <scripting blocked="false" disabled="false"/>
    <construction title="" author="" date="""#
    );

    for item in rendered {
        match (item) {
            Rendered::Point(pt) => {
                content += &format!(
                    "<element type=\"point\" label=\"{}\">
                <show object=\"true\" label=\"true\"/>
                <objColor r=\"21\" g=\"101\" b=\"192\" alpha=\"0\"/>
                <layer val=\"0\"/>
                <labelMode val=\"0\"/>
                <animation step=\"0.1\" speed=\"1\" type=\"1\" playing=\"false\"/>
                <pointSize val=\"5\"/>
                <pointStyle val=\"0\"/>
                <coords x=\"{}\" y=\"{}\" z=\"1\"/>
                </element>",
                    pt.label, pt.position.real, pt.position.imaginary
                );
            }
            Rendered::Line(ln) => {
                // "A" and "B" should refer to points defining the line
                content += &format!("<command name=\"Line\">
                <input a0=\"A\" a1=\"B\"/> 
                <output a0=\"{}\"/>
            </command>
            <element type=\"line\" label=\"{}\">
                <show object=\"true\" label=\"false\"/>
                <objColor r=\"97\" g=\"97\" b=\"97\" alpha=\"0\"/>
                <layer val=\"0\"/>
                <labelMode val=\"0\"/>
                <lineStyle thickness=\"5\" type=\"0\" typeHidden=\"1\" opacity=\"204\"/>
                <eqnStyle style=\"explicit\"/>
                <coords x=\"{}\" y=\"{}\" z=\"{}\"/>
            </element>", ln.label, );
            }
        }
    }

    let mut file = File::create(target).unwrap();
    let mut writer = ZipWriter::new(&file);
    writer
        .start_file("geo.xml", FileOptions::default())
        .unwrap();
    writer.write(content.as_bytes());
    writer.finish().unwrap();
}