package com.fy.navi.adas;

/**
 * 超级巡航
 *
 *一条路径线由N个段组成，每个段由N个链路组成，每个链路由N个点组成
 */
public class SuperCruiseJson {
    //数据可用
    private String dataAvailable;
    //车道数量
    private String laneCount;
    //道路类型
    private String roadCategory;
    //国家代码
    private String countryCode;
    //地图版本 年份
    private String mapVersionYear;
    //地图版本 季度
    private String mapVersionQuarter;
    //行驶方向
    private String drivingSideCategory;
    //是否受控
    private String controlledAccess;
    //划分道路类别
    private String dividedRoadCategory;
    //发布限速
    private String postedSpeedLimit;
    //推荐限速
    private String recommendedSpeedLimit;
    //限速是否保证
    private String speedLimitAssured;
    //条件限速
    private String conditionalSpeedLimit;
    //条件 速度类别
    private String conditionalSpeedCategory;
    //条件 速度类型
    private String conditionalSpeedType;
    //效果 限速
    private String effectSpeedLimit;
    //效果 速度类别
    private String effectiveSpeedCategory;
    //效果 速度类型
    private String effectiveSpeedType;
    //速度类别
    private String speedCategory;

    public String getDataAvailable() {
        return dataAvailable;
    }

    public void setDataAvailable(String dataAvailable) {
        this.dataAvailable = dataAvailable;
    }

    public String getLaneCount() {
        return laneCount;
    }

    public void setLaneCount(String laneCount) {
        this.laneCount = laneCount;
    }

    public String getRoadCategory() {
        return roadCategory;
    }

    public void setRoadCategory(String roadCategory) {
        this.roadCategory = roadCategory;
    }

    public String getCountryCode() {
        return countryCode;
    }

    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }

    public String getMapVersionYear() {
        return mapVersionYear;
    }

    public void setMapVersionYear(String mapVersionYear) {
        this.mapVersionYear = mapVersionYear;
    }

    public String getMapVersionQuarter() {
        return mapVersionQuarter;
    }

    public void setMapVersionQuarter(String mapVersionQuarter) {
        this.mapVersionQuarter = mapVersionQuarter;
    }

    public String getDrivingSideCategory() {
        return drivingSideCategory;
    }

    public void setDrivingSideCategory(String drivingSideCategory) {
        this.drivingSideCategory = drivingSideCategory;
    }

    public String getControlledAccess() {
        return controlledAccess;
    }

    public void setControlledAccess(String controlledAccess) {
        this.controlledAccess = controlledAccess;
    }

    public String getDividedRoadCategory() {
        return dividedRoadCategory;
    }

    public void setDividedRoadCategory(String dividedRoadCategory) {
        this.dividedRoadCategory = dividedRoadCategory;
    }

    public String getPostedSpeedLimit() {
        return postedSpeedLimit;
    }

    public void setPostedSpeedLimit(String postedSpeedLimit) {
        this.postedSpeedLimit = postedSpeedLimit;
    }

    public String getRecommendedSpeedLimit() {
        return recommendedSpeedLimit;
    }

    public void setRecommendedSpeedLimit(String recommendedSpeedLimit) {
        this.recommendedSpeedLimit = recommendedSpeedLimit;
    }

    public String getSpeedLimitAssured() {
        return speedLimitAssured;
    }

    public void setSpeedLimitAssured(String speedLimitAssured) {
        this.speedLimitAssured = speedLimitAssured;
    }

    public String getConditionalSpeedLimit() {
        return conditionalSpeedLimit;
    }

    public void setConditionalSpeedLimit(String conditionalSpeedLimit) {
        this.conditionalSpeedLimit = conditionalSpeedLimit;
    }

    public String getConditionalSpeedCategory() {
        return conditionalSpeedCategory;
    }

    public void setConditionalSpeedCategory(String conditionalSpeedCategory) {
        this.conditionalSpeedCategory = conditionalSpeedCategory;
    }

    public String getConditionalSpeedType() {
        return conditionalSpeedType;
    }

    public void setConditionalSpeedType(String conditionalSpeedType) {
        this.conditionalSpeedType = conditionalSpeedType;
    }

    public String getEffectSpeedLimit() {
        return effectSpeedLimit;
    }

    public void setEffectSpeedLimit(String effectSpeedLimit) {
        this.effectSpeedLimit = effectSpeedLimit;
    }

    public String getEffectiveSpeedCategory() {
        return effectiveSpeedCategory;
    }

    public void setEffectiveSpeedCategory(String effectiveSpeedCategory) {
        this.effectiveSpeedCategory = effectiveSpeedCategory;
    }

    public String getEffectiveSpeedType() {
        return effectiveSpeedType;
    }

    public void setEffectiveSpeedType(String effectiveSpeedType) {
        this.effectiveSpeedType = effectiveSpeedType;
    }

    public String getSpeedCategory() {
        return speedCategory;
    }

    public void setSpeedCategory(String speedCategory) {
        this.speedCategory = speedCategory;
    }
}
