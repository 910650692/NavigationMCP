package com.sgm.navi.service.adapter.position.bls.comm;

import android.content.Context;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationManager;
import android.os.SystemClock;
import android.text.format.Time;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.pos.model.LocGnss;
import com.sgm.navi.service.adapter.position.PositionConstant;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;


/**
 * 定位工具类
 */
public class LocationUtil {


    /**
     * 后端融合信号融合.
     *
     * @param location 位置信息
     * @return 高德位置信息对象
     */
    public static LocGnss getLocGnssByLocation(Location location, int satelliteNum) {
        final LocGnss locData = new LocGnss();
        locData.mode = 'A'; // 定位模式（'A','D','E','N'）。此字段暂时未使用，赋'A'
        switch (Objects.requireNonNull(location.getProvider())) {
            case LocationManager.GPS_PROVIDER:
                locData.sourType = 0; // 信号来源: 0 GPS定位  1 网络定位(包括室内定位和基站定位)
                locData.status = 'A';// GPS定位状态位。'A'：有效定位；
                break;
            case LocationManager.NETWORK_PROVIDER:
                locData.sourType = 1; // 信号来源: 0 GPS定位  1 网络定位(包括室内定位和基站定位)
                locData.status = 'A'; // GPS定位状态位。'V'：无效定位
                break;
            default:
                locData.sourType = 1; // 信号来源: 0 GPS定位  1 网络定位(包括室内定位和基站定位)
                locData.status = 'V'; // GPS定位状态位。'V'：无效定位
                break;
        }
        // 位置是否加密偏移: 0未偏移，1已经偏移  平板不会漂移   车机会漂移
        if (PositionConstant.isDrBack) {
            //isEncrypted=0，则 point 字段存储的是 WGS84 坐标；若 isEncrypted=1，则 point 存储的是 GCJ02 坐标
            if (CalibrationPackage.getInstance().navigationDeflectionEnable()) {
                //标定参数P_Navigation_Deflection_Enable=1表示地图需要使能偏转插件，输入的GPS为WGS84坐标系的数据
                locData.isEncrypted = 0;
            } else {
                //标定参数P_Navigation_Deflection_Enable=0表示地图不需要使能偏转插件，输入的GPS为GCJ02坐标系的数据；
                locData.isEncrypted = 1;
            }
        } else {
            locData.isEncrypted = 0;
        }
        locData.isNS = (byte) ((location.getLatitude() > 0) ? 'N' : 'S');
        locData.isEW = (byte) ((location.getLongitude() > 0) ? 'E' : 'W');
        float speed = (float) (location.getSpeed() * 3.6);
        if (speed > 0 && isDecimalTrailingZeros(speed)) {
//            Logger.d("TrailingZeros1","normal--" + speed);
            BigDecimal decimal1 = new BigDecimal(speed);
            BigDecimal decimal2 = new BigDecimal("0.01");
            BigDecimal decimal3 = decimal1.add(decimal2);
            speed = decimal3.floatValue();
//            Logger.d("TrailingZeros1","speed--" + speed);
        }
        locData.speed = speed;
//        Logger.d("TrailingZeros2",speed);
        locData.point = new Coord2DDouble(0.0, 0.0);
        locData.point.lon = location.getLongitude();
        locData.point.lat = location.getLatitude();
        if (location.hasBearing()) {
            locData.course = location.getBearing();
        }
        if (location.hasAltitude()) {
            locData.alt = (float) location.getAltitude();
        }
        // 卫星颗数从星历信息获取
        locData.num = satelliteNum;

        // TODO 参考GpsStatus.NmeaListener进行实现，未知使用-1.0f
        locData.hdop = -1.0f;
        locData.vdop = -1.0f;
        locData.pdop = -1.0f;
        // time
        Time time = new Time();
        time.set(location.getTime());
        locData.year = time.year;
        locData.month = time.month + 1;
        locData.day = time.monthDay;
        locData.hour = time.hour;
        locData.minute = time.minute;
        locData.second = time.second;
        if (location.hasAccuracy())
            locData.accuracy = location.getAccuracy(); // 精度半径。米
        // 推荐使用系统滴答数，而非信号源的时间戳
        return locData;
    }

    /**
     * @param num 判断数字小数点 后面是否全部是 0
     */
    private static boolean isDecimalTrailingZeros(float num) {
        String formatted = String.format("%.10f", num);
        return formatted.matches("\\d+\\.0+");
    }
}
