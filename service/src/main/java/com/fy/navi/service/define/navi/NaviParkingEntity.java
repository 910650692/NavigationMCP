package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.List;

/**
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 *  @author fy
 *  @version $Revision.*$
 */
public class NaviParkingEntity {
    private int mPoiType;           // poi 类型
    private String mPid;            // 父POI的Id
    private String mName;           // 名称
    private String mAddress;        // 地址
    private String mDistance;       // 距离（单位）
    private String mDis;            // 距离
    private GeoPoint mPoint;        // 经纬度
    private boolean mIsEndPoi;      //是否是终点
    private String mTag;            //停车场状态
    private String mNum;            //停车场数量展示
    private boolean mIsRecommend = false;   //是否是推荐
    private double mSortDis;

    private int mSpaceTotal;        //总车位数
    private int mSpaceFree;         //空闲车位数

    private GeoPoint mEnterPoint;
    private GeoPoint mExitPoint;

    // 充电列表站信息
    private List<ChargeInfo> mChargeInfoList;

    public int getPoiType() {
        return mPoiType;
    }

    /**
     * @param poiType poi 类型
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setPoiType(final int poiType) {
        this.mPoiType = poiType;
        return this;
    }

    public String getPid() {
        return mPid;
    }

    /**
     * @param pid 父POI的Id
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setPid(final String pid) {
        this.mPid = pid;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * @param name 名称
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getAddress() {
        return mAddress;
    }

    /**
     * @param address 地址
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setAddress(final String address) {
        this.mAddress = address;
        return this;
    }

    public String getDistance() {
        return mDistance;
    }

    /**
     * @param distance 距离
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setDistance(final String distance) {
        this.mDistance = distance;
        return this;
    }

    public String getDis() {
        return mDis;
    }

    public void setDis(final String dis) {
        this.mDis = dis;
    }

    public GeoPoint getPoint() {
        return mPoint;
    }

    /**
     * @param point 经纬度
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setPoint(final GeoPoint point) {
        this.mPoint = point;
        return this;
    }

    public boolean isEndPoi() {
        return mIsEndPoi;
    }

    /**
     * @param endPoi 是否是结束点
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setEndPoi(final boolean endPoi) {
        mIsEndPoi = endPoi;
        return this;
    }

    public String getTag() {
        return mTag;
    }

    /**
     * @param tag 标签
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setTag(final String tag) {
        this.mTag = tag;
        return this;
    }

    public String getNum() {
        return mNum;
    }

    /**
     * @param num 编号
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setNum(final String num) {
        this.mNum = num;
        return this;
    }

    public boolean isRecommend() {
        return mIsRecommend;
    }

    public void setRecommend(final boolean recommend) {
        mIsRecommend = recommend;
    }

    public double getSortDis() {
        return mSortDis;
    }

    public void setSortDis(final double sortDis) {
        this.mSortDis = sortDis;
    }

    public int getSpaceTotal() {
        return mSpaceTotal;
    }

    /**
     * @param spaceTotal spaceTotal
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setSpaceTotal(final int spaceTotal) {
        this.mSpaceTotal = spaceTotal;
        return this;
    }

    public int getSpaceFree() {
        return mSpaceFree;
    }

    public void setSpaceFree(final int spaceFree) {
        this.mSpaceFree = spaceFree;
    }

    public GeoPoint getEnterPoint() {
        return mEnterPoint;
    }

    public void setEnterPoint(final GeoPoint enterPoint) {
        this.mEnterPoint = enterPoint;
    }

    public GeoPoint getExitPoint() {
        return mExitPoint;
    }

    public void setExitPoint(final GeoPoint exitPoint) {
        this.mExitPoint = exitPoint;
    }

    public List<ChargeInfo> getChargeInfoList() {
        return mChargeInfoList;
    }

    /**
     * @param chargeInfoList chargeInfoList
     * @return NaviParkingEntity
     */
    public NaviParkingEntity setChargeInfoList(final List<ChargeInfo> chargeInfoList) {
        this.mChargeInfoList = chargeInfoList;
        return this;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviParkingEntity{" +
                "poiType=" + mPoiType +
                ", pid='" + mPid + '\'' +
                ", name='" + mName + '\'' +
                ", address='" + mAddress + '\'' +
                ", distance='" + mDistance + '\'' +
                ", dis='" + mDis + '\'' +
                ", point=" + mPoint +
                ", isEndPoi=" + mIsEndPoi +
                ", tag='" + mTag + '\'' +
                ", num='" + mNum + '\'' +
                ", isRecommend=" + mIsRecommend +
                ", sortDis=" + mSortDis +
                ", spaceTotal=" + mSpaceTotal +
                ", spaceFree=" + mSpaceFree +
                ", enterPoint=" + mEnterPoint +
                ", exitPoint=" + mExitPoint +
                '}';
    }
}
