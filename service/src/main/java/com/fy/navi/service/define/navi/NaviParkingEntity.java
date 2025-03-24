package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.bean.GeoPoint;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class NaviParkingEntity {
    private int poiType;           // poi 类型
    private String pid;            // 父POI的Id
    private String name;           // 名称
    private String address;        // 地址
    private String distance;       // 距离（单位）
    private String dis;            // 距离
    private GeoPoint point;        // 经纬度
    public boolean isEndPoi;      //是否是终点
    public String tag;            //停车场状态
    public String num;            //停车场数量展示
    public boolean isRecommend = false;   //是否是推荐
    private double sortDis;

    public int spaceTotal;        //总车位数
    public int spaceFree;         //空闲车位数

    private GeoPoint enterPoint;
    private GeoPoint exitPoint;
}
