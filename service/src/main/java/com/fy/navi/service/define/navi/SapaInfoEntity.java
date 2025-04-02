package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

/**
 * 服务区及其收费站实体类
 * @author fy
 * @version $Revision.*$
 */
public class SapaInfoEntity {
    @NaviConstant.SapaItemsType
    private int mType;//实体类类型
    private ArrayList<SAPAItem> mList;
    private short mRemainServiceAreaNum = 0;// 剩余服务区个数
    private long mRemainFreewayDistance = 0;// 剩余高速里程  （当前自车位到查询的最后一段高速的里程总和，单位米）
    private long mRemainFreewayTime = 0;// 剩余高速时间  （当前自车位到查询的最后一段高速的时间总和，单位秒）

    private ArrayList<Integer> mLaneTypes;//收费口车道类型 TollLaneType

    public ArrayList<Integer> getLaneTypes() {
        return mLaneTypes;
    }

    public void setLaneTypes(final ArrayList<Integer> laneTypes) {
        this.mLaneTypes = laneTypes;
    }

    public short getRemainServiceAreaNum() {
        return mRemainServiceAreaNum;
    }

    public void setRemainServiceAreaNum(final short remainServiceAreaNum) {
        this.mRemainServiceAreaNum = remainServiceAreaNum;
    }

    public long getRemainFreewayDistance() {
        return mRemainFreewayDistance;
    }

    public void setRemainFreewayDistance(final long remainFreewayDistance) {
        this.mRemainFreewayDistance = remainFreewayDistance;
    }

    public long getRemainFreewayTime() {
        return mRemainFreewayTime;
    }

    public void setRemainFreewayTime(final long remainFreewayTime) {
        this.mRemainFreewayTime = remainFreewayTime;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public ArrayList<SAPAItem> getList() {
        return mList;
    }

    public void setList(final ArrayList<SAPAItem> list) {
        this.mList = list;
    }

    //服务区详细信息配置
    public static class SAPAItem {
        private int mRemainDist;//到当前车位的剩余距离（单位米）
        private int mType;//类型
        private String mName;//名称
        private GeoPoint mPos;//位置
        private long mSapaDetail;//服务区详细信息：按位标识（0-没有，1-有），第0~6位分别表示加油站、餐饮、卫生间、汽修、购物、住宿、充电站
        private long mRemainTime;//当前车位距离到sapa的剩余时间（单位秒）
        private String mServicePOIID;//服务区设施ID
        private int mBuildingStatus;//服务区状态:0 非建设中（默认值），1 建设中，2 未调查 3 装修中 4 暂停营业

        public int getRemainDist() {
            return mRemainDist;
        }

        public void setRemainDist(final int remainDist) {
            this.mRemainDist = remainDist;
        }

        public int getType() {
            return mType;
        }

        public void setType(final int type) {
            this.mType = type;
        }

        public String getName() {
            return mName;
        }

        public void setName(final String name) {
            this.mName = name;
        }

        public GeoPoint getPos() {
            return mPos;
        }

        public void setPos(final GeoPoint pos) {
            this.mPos = pos;
        }

        public long getSapaDetail() {
            return mSapaDetail;
        }

        public void setSapaDetail(final long sapaDetail) {
            this.mSapaDetail = sapaDetail;
        }

        public long getRemainTime() {
            return mRemainTime;
        }

        public void setRemainTime(final long remainTime) {
            this.mRemainTime = remainTime;
        }

        public String getServicePOIID() {
            return mServicePOIID;
        }

        public void setServicePOIID(final String servicePOIID) {
            this.mServicePOIID = servicePOIID;
        }

        public int getBuildingStatus() {
            return mBuildingStatus;
        }

        public void setBuildingStatus(final int buildingStatus) {
            this.mBuildingStatus = buildingStatus;
        }

        @NonNull
        @Override
        public String toString() {
            return "SAPAItem{" +
                    "remainDist=" + mRemainDist +
                    ", type=" + mType +
                    ", name='" + mName + '\'' +
                    ", pos=" + mPos +
                    ", sapaDetail=" + mSapaDetail +
                    ", remainTime=" + mRemainTime +
                    ", servicePOIID='" + mServicePOIID + '\'' +
                    ", buildingStatus=" + mBuildingStatus +
                    '}';
        }
    }

    @NonNull
    @Override
    public String toString() {
        return "SapaInfoEntity{" +
                "type=" + mType +
                ", list=" + mList +
                ", remainServiceAreaNum=" + mRemainServiceAreaNum +
                ", remainFreewayDistance=" + mRemainFreewayDistance +
                ", remainFreewayTime=" + mRemainFreewayTime +
                ", laneTypes=" + mLaneTypes +
                '}';
    }
}
