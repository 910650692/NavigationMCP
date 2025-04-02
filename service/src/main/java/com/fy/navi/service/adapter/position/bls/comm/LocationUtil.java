package com.fy.navi.service.adapter.position.bls.comm;

import android.content.Context;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationManager;
import android.os.SystemClock;
import android.text.format.Time;

import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.pos.model.LocDataType;
import com.autonavi.gbl.pos.model.LocGnss;
import com.fy.navi.service.adapter.position.PositionConstant;

import java.math.BigInteger;
import java.util.Objects;


/**
 * 定位工具类
 */
public class LocationUtil {

    public static String TAG = "LocationUtil";
    /**
     * 圆周率
     */
    public static double pi = 3.14159265358979324;
    public static double x_pi = pi * 3000 / 180;
    /**
     * 地球长半轴距离，米
     */
    public static double a = 6378245;
    /**
     * 地球扁率
     */
    public static double ee = 0.00669342162296594323;

    private static LocationUtil sInstance;
    private LocationManager mLocationManager;
    private Criteria mCriteria;
    private String mBestProvidor;

    private LocationUtil() {

    }

    public static LocationUtil getInstance() {
        if (null == sInstance) {
            sInstance = new LocationUtil();
        }
        return sInstance;
    }

    /**
     * 获取定位信息
     *
     * @return Location对象
     */
    public Location getLocation(Context context) {
        if (null == mLocationManager) {
            mLocationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
            if (null == mLocationManager) {
                return null;
            }
        }
        if (null == mCriteria) {
            mCriteria = new Criteria();
            mCriteria.setAccuracy(Criteria.ACCURACY_FINE);
            mCriteria.setSpeedRequired(false);
            mCriteria.setCostAllowed(false);
            mCriteria.setBearingRequired(false);
            mCriteria.setAltitudeRequired(false);
            mCriteria.setPowerRequirement(Criteria.POWER_LOW);
        }
        if (null == mBestProvidor) {
            mBestProvidor = mLocationManager.getBestProvider(getCriteria(), true);
            if (null == mBestProvidor) {
                return null;
            }
        }
        Location location = null;
        try {
            location = mLocationManager.getLastKnownLocation(mBestProvidor);
        } catch (SecurityException ex) {
        } catch (IllegalArgumentException ex) {
        } catch (Exception ex) {
        }
        return location;
    }

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
        if(PositionConstant.isDrBack){
            locData.isEncrypted = 1;
        }else {
            locData.isEncrypted = 0;
        }
        locData.isNS = (byte) ((location.getLatitude() > 0) ? 'N' : 'S');
        locData.isEW = (byte) ((location.getLongitude() > 0) ? 'E' : 'W');
        locData.speed = (float) (location.getSpeed() * 3.6);
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
        locData.tickTime = BigInteger.valueOf(SystemClock.elapsedRealtime());
        return locData;
    }

    private Criteria getCriteria() {
        Criteria criteria = new Criteria();
        criteria.setAccuracy(Criteria.ACCURACY_FINE);
        criteria.setSpeedRequired(false);
        criteria.setCostAllowed(false);
        criteria.setBearingRequired(false);
        criteria.setAltitudeRequired(false);
        criteria.setPowerRequirement(Criteria.POWER_LOW);
        return criteria;
    }

    /**
     * 高德转84
     *
     * @param lat 经度
     * @param lon 纬度
     * @return double[]    转换后的[lat, lon]
     */
    public static double[] GCJ02_To_WGS84(double lat, double lon) {
        double[] gps = transform(lat, lon);
        double latitude = lat * 2 - gps[0];
        double longitude = lon * 2 - gps[1];
        return new double[]{longitude, latitude};
    }

    public static double[] transform(double lat, double lon) {
        if (outOfChina(lat, lon)) {
            return new double[]{lat, lon};
        }
        double dLat = transformLat(lon - 105.0, lat - 35.0);
        double dLon = transformLon(lon - 105.0, lat - 35.0);
        double radLat = lat / 180.0 * pi;
        double magic = Math.sin(radLat);
        magic = 1 - ee * magic * magic;
        double sqrtMagic = Math.sqrt(magic);
        dLat = (dLat * 180.0) / ((a * (1 - ee)) / (magic * sqrtMagic) * pi);
        dLon = (dLon * 180.0) / (a / sqrtMagic * Math.cos(radLat) * pi);
        double mgLat = lat + dLat;
        double mgLon = lon + dLon;
        return new double[]{mgLat, mgLon};
    }

    public static double transformLat(double x, double y) {
        double ret = -100.0 + 2.0 * x + 3.0 * y + 0.2 * y * y + 0.1 * x * y + 0.2 * Math.sqrt(Math.abs(x));
        ret += (20.0 * Math.sin(6.0 * x * pi) + 20.0 * Math.sin(2.0 * x * pi)) * 2.0 / 3.0;
        ret += (20.0 * Math.sin(y * pi) + 40.0 * Math.sin(y / 3.0 * pi)) * 2.0 / 3.0;
        ret += (160.0 * Math.sin(y / 12.0 * pi) + 320 * Math.sin(y * pi / 30.0)) * 2.0 / 3.0;
        return ret;
    }

    public static double transformLon(double x, double y) {
        double ret = 300.0 + x + 2.0 * y + 0.1 * x * x + 0.1 * x * y + 0.1 * Math.sqrt(Math.abs(x));
        ret += (20.0 * Math.sin(6.0 * x * pi) + 20.0 * Math.sin(2.0 * x * pi)) * 2.0 / 3.0;
        ret += (20.0 * Math.sin(x * pi) + 40.0 * Math.sin(x / 3.0 * pi)) * 2.0 / 3.0;
        ret += (150.0 * Math.sin(x / 12.0 * pi) + 300.0 * Math.sin(x / 30.0 * pi)) * 2.0 / 3.0;
        return ret;
    }

    /**
     * 判断经纬度是否在中国
     *
     * @param lat 经度
     * @param lon 纬度
     * @return
     */
    public static boolean outOfChina(double lat, double lon) {
        if (lon < 72.004 || lon > 137.8347)
            return true;
        if (lat < 0.8293 || lat > 55.8271)
            return true;
        return false;
    }
}
