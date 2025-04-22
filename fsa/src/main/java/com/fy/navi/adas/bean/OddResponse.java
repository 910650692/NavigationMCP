package com.fy.navi.adas.bean;

public class OddResponse {
    /**
     * matching接口版本
     */
    private String api_version;
    /**
     * 车端数据catalog
     */
    private String catalog_name;
    /**
     * DDLD landtopo数据版本
     */
    private String landtopo_data_version;
    /**
     * 异常CODE对应的信息，正常为空
     */
    private String message;//错误信息
    /**
     * 云端算路异常状态CODE
     * 0 正常，请求版本与云端数据版本一致
     * 1 错误，请求版本与云端数据版本不一致
     * 2 错误，请求的catalog不存在或不支持云端算路服务
     * 3 没有权限
     * 4 mode不正确
     * 5 请求体异常
     * 6 服务内部异常
     * 502  odd close获取失败
     * 503 冷加载地图获取失败
     */
    private int status_code;//状态码
    /**
     * matching服务序号，标识云端匹配算路请求唯一ID
     */
    private String uuid;//uuid
    /**
     * 车端数据version
     */
    private int version;
    /**
     * 基于SD的道路matching类型分段
     *
     * 一条路径线由N个段组成，每个段由N个链路组成，每个链路由N个点组成
     */
    private SwitchSegments[] switch_segments;

    public String getApi_version() {
        return api_version;
    }

    public void setApi_version(String api_version) {
        this.api_version = api_version;
    }

    public String getCatalog_name() {
        return catalog_name;
    }

    public void setCatalog_name(String catalog_name) {
        this.catalog_name = catalog_name;
    }

    public String getLandtopo_data_version() {
        return landtopo_data_version;
    }

    public void setLandtopo_data_version(String landtopo_data_version) {
        this.landtopo_data_version = landtopo_data_version;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public int getStatus_code() {
        return status_code;
    }

    public void setStatus_code(int status_code) {
        this.status_code = status_code;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public SwitchSegments[] getSwitch_segments() {
        return switch_segments;
    }

    public void setSwitch_segments(SwitchSegments[] switch_segments) {
        this.switch_segments = switch_segments;
    }
}
