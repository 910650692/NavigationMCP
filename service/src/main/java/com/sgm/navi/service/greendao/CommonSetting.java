package com.sgm.navi.service.greendao;

import org.greenrobot.greendao.annotation.Entity;
import org.greenrobot.greendao.annotation.Generated;
import org.greenrobot.greendao.annotation.Id;
import org.greenrobot.greendao.annotation.Keep;
import org.greenrobot.greendao.annotation.Property;
import org.greenrobot.greendao.annotation.Unique;

import java.util.Date;

@Entity
public class CommonSetting {

    @Id(autoincrement = true)
    private Long mId;

    @Unique
    @Property(nameInDb = "name")
    private String mName;

    @Property(nameInDb = "value")
    private String mValue;

    @Property(nameInDb = "updateTime")
    private Date mUpdateTime;

    @Keep
    public CommonSetting(final Long id, final String name, final String value, final Date updateTime) {
        this.mId = id;
        this.mName = name;
        this.mValue = value;
        this.mUpdateTime = updateTime;
    }

    @Generated(hash = 2144871971)
    public CommonSetting() {
    }

    public Long getMId() {
        return this.mId;
    }

    public void setMId(final Long id) {
        this.mId = id;
    }

    public String getMName() {
        return this.mName;
    }

    public void setMName(final String name) {
        this.mName = name;
    }

    public String getMValue() {
        return this.mValue;
    }

    public void setMValue(final String value) {
        this.mValue = value;
    }

    public Date getMUpdateTime() {
        return this.mUpdateTime;
    }

    public void setMUpdateTime(final Date updateTime) {
        this.mUpdateTime = updateTime;
    }
}
