package com.fy.navi.vrbridge.bean;

import android.text.TextUtils;

import com.fy.navi.vrbridge.IVrBridgeConstant;

import java.util.Map;

public class VoiceSearchConditions {

    /**
     * 距离条件.
     * 1.NEAREST：最近/最近的
     * 2.FURTHEST：最远/最远的
     * 3.NEARBY：附近/附近的
     * 4.可以为区间值，[,2000] 两公里范围内.
     */
    private String mDistance;
    /**
     * 价格条件.
     * 1.CHEAP:便宜的/优惠的
     * 2.区间值
     *   [100,200]：100到200
     *   [100，]：100以上
     *   [200，200]：200左右
     */
    private String mPrice;
    /**
     * HIGH:好评的/评价高的.
     */
    private String mRate;
    /**
     * 可提供的服务，如可预订/有团购/可选座/IMAX厅.
     */
    private String mService;
    /**
     * 酒店星级、景区等级、
     * 5A/4A/3A/2A/1A/5星/4星/3星/2星/1星.
     */
    private String mLevel;
    /**
     * 中心点，如果存在需要先搜索中心点，然后以中心点执行周边搜.
     */
    private String mCenter;


    public VoiceSearchConditions() {
        mDistance = "";
        mPrice = "";
        mRate = "";
        mService = "";
        mLevel = "";
        mCenter = "";
    }

    /**
     * 解析语音深度搜索筛选条件.
     *
     * @param conditionMap Map，筛选条件组合.
     */
    public void parseConditionMap(final Map<String, String> conditionMap) {
        if (null == conditionMap || conditionMap.isEmpty()) {
            return;
        }

        //距离
        String distance = conditionMap.getOrDefault(IVrBridgeConstant.ConditionKey.DISTANCE, "");
        if (TextUtils.isEmpty(distance)) {
            mDistance = "";
        } else if (distance.contains("[")) {
            //周边范围搜
            distance = distance.substring(1, distance.length() - 1);
            final String[] distanceArray = distance.split(",");
            if (distanceArray.length >= 2) {
                mDistance = distanceArray[1];
            } else if (distanceArray.length == 1) {
                mDistance = distanceArray[0];
            } else {
                mDistance = "";
            }
        } else if (!IVrBridgeConstant.DistanceValue.NEAREST.equals(distance)) {
            //简单周边
            mDistance = IVrBridgeConstant.DistanceValue.NEARBY;
        }

        //价格
        mPrice = conditionMap.getOrDefault(IVrBridgeConstant.ConditionKey.PRICE, "");
        if (!IVrBridgeConstant.PRICE_CHEAP.equals(mPrice)) {
            mPrice = "";
        }

        mRate = conditionMap.getOrDefault(IVrBridgeConstant.ConditionKey.RATE, "");
        if (!IVrBridgeConstant.RATE_HIGH.equals(mRate)) {
            mRate = "";
        }

        mLevel = conditionMap.getOrDefault(IVrBridgeConstant.ConditionKey.LEVEL, "");
        mCenter = conditionMap.getOrDefault(IVrBridgeConstant.ConditionKey.CENTER, "");
    }

    public String getDistance() {
        return mDistance;
    }

    public void setDistance(final String distance) {
        this.mDistance = distance;
    }

    public String getPrice() {
        return mPrice;
    }

    public void setPrice(final String price) {
        this.mPrice = price;
    }

    public String getRate() {
        return mRate;
    }

    public void setRate(final String rate) {
        this.mRate = rate;
    }

    public String getService() {
        return mService;
    }

    public void setService(final String service) {
        this.mService = service;
    }

    public String getLevel() {
        return mLevel;
    }

    public void setLevel(final String level) {
        this.mLevel = level;
    }

    public String getCenter() {
        return mCenter;
    }

    public void setCenter(final String center) {
        this.mCenter = center;
    }

    @Override
    public String toString() {
        return "VoiceSearchConditions{" +
                "mDistance='" + mDistance + '\'' +
                ", mPrice='" + mPrice + '\'' +
                ", mRate='" + mRate + '\'' +
                ", mService='" + mService + '\'' +
                ", mLevel='" + mLevel + '\'' +
                ", mCenter='" + mCenter + '\'' +
                '}';
    }

}
