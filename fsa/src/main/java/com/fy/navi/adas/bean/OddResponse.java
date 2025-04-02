package com.fy.navi.adas.bean;

public class OddResponse {
    private String api_version;
    private String catalog_name;
    private String landtopo_data_version;
    private String message;
    private int status_code;
    private String uuid;
    private int version;
    private String[] switch_segments;

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

    public String[] getSwitch_segments() {
        return switch_segments;
    }

    public void setSwitch_segments(String[] switch_segments) {
        this.switch_segments = switch_segments;
    }
}
