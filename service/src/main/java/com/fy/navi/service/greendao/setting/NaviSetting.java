package com.fy.navi.service.greendao.setting;

import org.greenrobot.greendao.annotation.Entity;
import org.greenrobot.greendao.annotation.Id;
import org.greenrobot.greendao.annotation.Property;
import org.greenrobot.greendao.annotation.Unique;

import java.util.Date;
import org.greenrobot.greendao.annotation.Generated;


@Entity
public class NaviSetting {

    @Id(autoincrement = true)
    public Long id;

    @Unique
    @Property(nameInDb = "name")
    private String name;

    @Property(nameInDb = "value")
    private String value;

    @Property(nameInDb = "updateTime")
    private Date updateTime;

    @Generated(hash = 1840668051)
    public NaviSetting(Long id, String name, String value, Date updateTime) {
        this.id = id;
        this.name = name;
        this.value = value;
        this.updateTime = updateTime;
    }

    @Generated(hash = 2089403719)
    public NaviSetting() {
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Date getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
