package com.fy.navi.service.adapter.search.cloudByPatac.req;

import com.fy.navi.service.adapter.search.cloudByPatac.bean.SavedStations;
import com.patac.netlib.convert.Header;

import java.util.ArrayList;

public class StationReq extends BaseReq{
    @Header
    private Long timestamp;
    // 充电站列表查询 参数
    private String areaCode;// 城市
    private String lat; // 纬度
    private String lng; // 经度
    private String keyWords; // 地址/充电站名称 模糊查询
    private String from; // 默认为第一页
    private String size; // 默认查询前100条
    // 充电站列表查询 参数
    private String vehicleBrand;//BUICK, CHEVY, CADILLAC, CADILLAC IQ
    private Integer pageSize; // 分页大小
    private Integer pageNum; // 分页页码
    // 修改预约 参数
    private String preNum;// 预约单号
    private Integer status;// 预约单状态：1：已预约 2：已完成 3：已取消
    private String cancelReason;// 取消理由
    // 创建公桩预约
    private String operatorId; // 运营商Id
    private String stationId; // 充电站Id
    private String connectorId; // 充电设备接口编码
    private Integer brandId; // 1.别克 2.凯迪 3.雪佛兰 4.奥特能 5.泛亚
    private String remark; // 备注
    private String source; // 来源手机应用：APP微信小程序：WEAPP 车机端：CARAPP
    // 更新用户收藏充电站
    private String channel;// 数据上传方（可选值：CSM, MOBILE）CSM：车机端，MOBILE：手机端
    private String updateType; // 更新模式，增量or全量（可选值：INCREMENT, FULL)
    private String savedStations;// 用户收藏的充电站信息数组
    // 获取充电桩信息
    private String equipmentId; // 桩id

    public StationReq(String apiVersion) {
        super(apiVersion);
    }

    public StationReq(String apiVersion, String idpUserId) {
        super(apiVersion, idpUserId);
    }

    public String getAreaCode() {
        return areaCode;
    }

    public StationReq setAreaCode(String areaCode) {
        this.areaCode = areaCode;
        return this;
    }

    public String getLat() {
        return lat;
    }

    public StationReq setLat(String lat) {
        this.lat = lat;
        return this;
    }

    public String getLng() {
        return lng;
    }

    public StationReq setLng(String lng) {
        this.lng = lng;
        return this;
    }

    public String getKeyWords() {
        return keyWords;
    }

    public StationReq setKeyWords(String keyWords) {
        this.keyWords = keyWords;
        return this;
    }

    public String getFrom() {
        return from;
    }

    public StationReq setFrom(String from) {
        this.from = from;
        return this;
    }

    public String getSize() {
        return size;
    }

    public StationReq setSize(String size) {
        this.size = size;
        return this;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public StationReq setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
        return this;
    }

    public Integer getPageSize() {
        return pageSize;
    }

    public StationReq setPageSize(Integer pageSize) {
        this.pageSize = pageSize;
        return this;
    }

    public Integer getPageNum() {
        return pageNum;
    }

    public StationReq setPageNum(Integer pageNum) {
        this.pageNum = pageNum;
        return this;
    }

    public String getVehicleBrand() {
        return vehicleBrand;
    }

    public StationReq setVehicleBrand(String vehicleBrand) {
        this.vehicleBrand = vehicleBrand;
        return this;
    }

    public void setAccessToken(String accessToken) {
        super.setAccessToken(accessToken);
    }

    public String getOperatorId() {
        return operatorId;
    }

    public StationReq setOperatorId(String operatorId) {
        this.operatorId = operatorId;
        return this;
    }

    public String getStationId() {
        return stationId;
    }

    public StationReq setStationId(String stationId) {
        this.stationId = stationId;
        return this;
    }

    public String getChannel() {
        return channel;
    }

    public StationReq setChannel(String channel) {
        this.channel = channel;
        return this;
    }

    public String getSavedStations() {
        return savedStations;
    }

    public StationReq setSavedStations(String savedStations) {
        this.savedStations = savedStations;
        return this;
    }

    public String getUpdateType() {
        return updateType;
    }

    public StationReq setUpdateType(String updateType) {
        this.updateType = updateType;
        return this;
    }
}
