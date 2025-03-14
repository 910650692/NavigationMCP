package com.fy.navi.service.define.aos;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RestrictedAreaDetail implements Serializable {
    private long requestId;
    private int cityCode;           //1100
    private String cityName;        //北京市
    private String title;           //北京市限行政策
    public String policyname;       //二环及以内外地机动车限行
    public int ruleid;              //1766888
    public int ring;                //0
    public int effect;              //1
    public int local;               //2
    public int vehicle;             //1
    public String time;             //2021年11月1日起，工作日和周末全天限行（节假日限制）
    public String summary;          //二环路及以内道路（不含外侧辅路）
    public String desc;             //外地机动车禁行
    public String otherdesc;        //“”

    public RestrictedAreaDetail() {
    }

    public RestrictedAreaDetail(long requestId, int cityCode, String cityName, String title,
                                String policyname, int ruleid, int ring, int effect, int local,
                                int vehicle, String time, String summary, String desc, String otherdesc) {
        this.requestId = requestId;
        this.cityCode = cityCode;
        this.cityName = cityName;
        this.title = title;
        this.policyname = policyname;
        this.ruleid = ruleid;
        this.ring = ring;
        this.effect = effect;
        this.local = local;
        this.vehicle = vehicle;
        this.time = time;
        this.summary = summary;
        this.desc = desc;
        this.otherdesc = otherdesc;
    }
}
