package com.sgm.navi.service.define.cruise;


import java.util.ArrayList;

public class CruiseInfoEntity {
    //onUpdateCruiseInfo 巡航过程中传出巡航状态下的信息
    public String roadName;//当前道路名
    public int roadClass;//当前道路等级
    public int distance; // 车距电子眼距离 单位:米

    public ArrayList<Short> getSpeed() {
        return speed;
    }

    public void setSpeed(ArrayList<Short> speed) {
        this.speed = speed;
    }

    //按照文档示例，暂时提取巡航电子眼限速值，后续需要额外信息要看具体业务场景
    private ArrayList<Short> speed;

    public String getRoadName() {
        return roadName;
    }

    public void setRoadName(String roadName) {
        this.roadName = roadName;
    }

    public int getRoadClass() {
        return roadClass;
    }

    public void setRoadClass(int roadClass) {
        this.roadClass = roadClass;
    }
}
