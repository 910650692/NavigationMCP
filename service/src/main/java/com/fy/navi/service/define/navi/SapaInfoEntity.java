package com.fy.navi.service.define.navi;


import androidx.annotation.NonNull;

import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

/**
 * 服务区及其收费站实体类
 */
public class SapaInfoEntity {
    @NaviConstant.SapaItemsType
    private int type;//实体类类型
    private ArrayList<SAPAItem> list;
    private short remainServiceAreaNum = 0;// 剩余服务区个数
    private long remainFreewayDistance = 0;// 剩余高速里程  （当前自车位到查询的最后一段高速的里程总和，单位米）
    private long remainFreewayTime = 0;// 剩余高速时间  （当前自车位到查询的最后一段高速的时间总和，单位秒）

    private ArrayList<Integer> laneTypes;//收费口车道类型 TollLaneType

    public ArrayList<Integer> getLaneTypes() {
        return laneTypes;
    }

    public void setLaneTypes(ArrayList<Integer> laneTypes) {
        this.laneTypes = laneTypes;
    }

    public short getRemainServiceAreaNum() {
        return remainServiceAreaNum;
    }

    public void setRemainServiceAreaNum(short remainServiceAreaNum) {
        this.remainServiceAreaNum = remainServiceAreaNum;
    }

    public long getRemainFreewayDistance() {
        return remainFreewayDistance;
    }

    public void setRemainFreewayDistance(long remainFreewayDistance) {
        this.remainFreewayDistance = remainFreewayDistance;
    }

    public long getRemainFreewayTime() {
        return remainFreewayTime;
    }

    public void setRemainFreewayTime(long remainFreewayTime) {
        this.remainFreewayTime = remainFreewayTime;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public ArrayList<SAPAItem> getList() {
        return list;
    }

    public void setList(ArrayList<SAPAItem> list) {
        this.list = list;
    }

    //服务区详细信息配置
    public static class SAPAItem {
        private int remainDist;//到当前车位的剩余距离（单位米）
        private int type;//类型
        private String name;//名称
        private GeoPoint pos;//位置
        private long sapaDetail;//服务区详细信息：按位标识（0-没有，1-有），第0~6位分别表示加油站、餐饮、卫生间、汽修、购物、住宿、充电站
        private long remainTime;//当前车位距离到sapa的剩余时间（单位秒）
        private String servicePOIID;//服务区设施ID
        private int buildingStatus;//服务区状态:0 非建设中（默认值），1 建设中，2 未调查 3 装修中 4 暂停营业

        public int getRemainDist() {
            return remainDist;
        }

        public void setRemainDist(int remainDist) {
            this.remainDist = remainDist;
        }

        public int getType() {
            return type;
        }

        public void setType(int type) {
            this.type = type;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public GeoPoint getPos() {
            return pos;
        }

        public void setPos(GeoPoint pos) {
            this.pos = pos;
        }

        public long getSapaDetail() {
            return sapaDetail;
        }

        public void setSapaDetail(long sapaDetail) {
            this.sapaDetail = sapaDetail;
        }

        public long getRemainTime() {
            return remainTime;
        }

        public void setRemainTime(long remainTime) {
            this.remainTime = remainTime;
        }

        public String getServicePOIID() {
            return servicePOIID;
        }

        public void setServicePOIID(String servicePOIID) {
            this.servicePOIID = servicePOIID;
        }

        public int getBuildingStatus() {
            return buildingStatus;
        }

        public void setBuildingStatus(int buildingStatus) {
            this.buildingStatus = buildingStatus;
        }

        @Override
        public String toString() {
            return "SAPAItem{" +
                    "remainDist=" + remainDist +
                    ", type=" + type +
                    ", name='" + name + '\'' +
                    ", pos=" + pos +
                    ", sapaDetail=" + sapaDetail +
                    ", remainTime=" + remainTime +
                    ", servicePOIID='" + servicePOIID + '\'' +
                    ", buildingStatus=" + buildingStatus +
                    '}';
        }
    }

    @Override
    public String toString() {
        return "SapaInfoEntity{" +
                "type=" + type +
                ", list=" + list +
                ", remainServiceAreaNum=" + remainServiceAreaNum +
                ", remainFreewayDistance=" + remainFreewayDistance +
                ", remainFreewayTime=" + remainFreewayTime +
                ", laneTypes=" + laneTypes +
                '}';
    }
}
