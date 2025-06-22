package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

/**
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 *  @author sgm
 *  @version $Revision.*$
 */

public class NaviParamEntity {
    private int mType;//类型
    private int mStyle = 2;//播报类型
    private boolean mEnableADCode = true;
    private int mFatiguedTTS = 0;
    private boolean mIsDay = true;

    public int getType() {
        return mType;
    }

    /**
     * @param type type
     * @return 为了支持链式调用，返回当前对象
     */
    public NaviParamEntity setType(final int type) {
        this.mType = type;
        return this;
    }

    public int getStyle() {
        return mStyle;
    }

    /**
     * @param style style
     * @return 为了支持链式调用，返回当前对象
     */
    public NaviParamEntity setStyle(final int style) {
        this.mStyle = style;
        return this;
    }

    public boolean isEnableADCode() {
        return mEnableADCode;
    }

    /**
     * @param enableADCode enableADCode
     * @return 为了支持链式调用，返回当前对象
     */
    public NaviParamEntity setEnableADCode(final boolean enableADCode) {
        this.mEnableADCode = enableADCode;
        return this;
    }

    public int getFatiguedTTS() {
        return mFatiguedTTS;
    }

    /**
     * @param fatiguedTTS fatiguedTTS
     * @return 为了支持链式调用，返回当前对象
     */
    public NaviParamEntity setFatiguedTTS(final int fatiguedTTS) {
        this.mFatiguedTTS = fatiguedTTS;
        return this;
    }

    public boolean isDay() {
        return mIsDay;
    }

    /**
     * @param day
     * @return 为了支持链式调用，返回当前对象
     */
    public NaviParamEntity setDay(final boolean day) {
        mIsDay = day;
        return this;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviParamEntity{" +
                "type=" + mType +
                ", style=" + mStyle +
                ", enableADCode=" + mEnableADCode +
                ", fatiguedTTS=" + mFatiguedTTS +
                ", isDay=" + mIsDay +
                '}';
    }
}
