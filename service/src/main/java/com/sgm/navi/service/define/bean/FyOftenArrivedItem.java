package com.sgm.navi.service.define.bean;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/26
 */
@Getter
@Setter
public class FyOftenArrivedItem {
    public String wstrPoiID; // POI 的唯一标识符
    public String wstrPoiName; // POI 名称
    public String wstrPoiType; // POI 类型，参考 typecode 定义
    public String wstrAddress; // POI 地址1
    public GeoPoint stDisplayCoord; // 显示坐标
    public GeoPoint stNaviCoord; // 导航坐标
    public String parent; // 终点的父POIID
    public String towardsAngle; // POI门脸朝向
    public String floorNo; // 终点的楼层信息
    public int childType; // 终点的父POI与子POI的关系类型
    public int endPoiExtension; // 该POI有扩展信息 0x0001:标识multi_navi多到达点，强制查询POI 0x0002:标示shop_mark底商，强制查询POI1
    public long topTime; // 被置顶时微秒时间戳，没有被置顶过该值为0（只适用于收藏类常去地点）
//    public DateTime dateTime; // 添加时间
    public ArrayList<Long> uTimeList; // 历史记录时间时间戳(单位:秒)列表

    @Override
    public String toString() {
        return "FyOftenArrivedItem{" +
                "wstrPoiID='" + wstrPoiID + '\'' +
                ", wstrPoiName='" + wstrPoiName + '\'' +
                ", wstrPoiType='" + wstrPoiType + '\'' +
                ", wstrAddress='" + wstrAddress + '\'' +
                ", stDisplayCoord=" + stDisplayCoord +
                ", stNaviCoord=" + stNaviCoord +
                ", parent='" + parent + '\'' +
                ", towardsAngle='" + towardsAngle + '\'' +
                ", floorNo='" + floorNo + '\'' +
                ", childType=" + childType +
                ", endPoiExtension=" + endPoiExtension +
                ", topTime=" + topTime +
                ", uTimeList=" + uTimeList +
                '}';
    }
}
