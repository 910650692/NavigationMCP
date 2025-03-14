package com.fy.navi.vrbridge;

import android.content.Context;
import android.text.TextUtils;

import com.baidu.oneos.protocol.bean.PoiBean;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.favorite.Favorite;

import java.util.ArrayList;
import java.util.List;

/**
 * 将Package回调的结果转换为语音内部定义的类.
 */
public class VoiceConvertUtil {

    /**
     * 转换搜索结果，回调给百度语音.
     * @return
     */
    public static final List<PoiBean> convertSearchResult(List<PoiInfoEntity> poiInfoEntityList) {
        List<PoiBean> poiBeanList = new ArrayList<>();
        if (null == poiInfoEntityList || poiInfoEntityList.isEmpty()) {
            return poiBeanList;
        }

        for (PoiInfoEntity poiInfo : poiInfoEntityList) {
            if (null == poiInfo || TextUtils.isEmpty(poiInfo.getPid())) {
                continue;
            }

            PoiBean poiBean = new PoiBean();
            poiBean.setName(poiInfo.getName());
            poiBean.setAddress(poiInfo.getAddress());
            poiBean.setDistance(poiInfo.getDistance());
            poiBeanList.add(poiBean);
        }

        return poiBeanList;
    }

    /**
     * 将语音传入的
     * @param routeType
     * @return
     */
    public static final RoutePreferenceID convertToAMapPrefer(String routeType) {
        RoutePreferenceID routePreferenceID;
        switch (routeType) {
            case IVrBridgeConstant.RouteType.AVOID_JAM:
                routePreferenceID = RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
                break;
            case IVrBridgeConstant.RouteType.FAST_SPEED:
                routePreferenceID = RoutePreferenceID.PREFERENCE_FASTESTSPEED;
                break;
            case IVrBridgeConstant.RouteType.FIRST_HIGHWAY:
                routePreferenceID = RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
                break;
            case IVrBridgeConstant.RouteType.LESS_CHARGE:
                routePreferenceID = RoutePreferenceID.PREFERENCE_LESSCHARGE;
                break;
            case IVrBridgeConstant.RouteType.NOT_HIGHWAY:
                routePreferenceID = RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
                break;
            case IVrBridgeConstant.RouteType.PREFER_RECOMMEND:
            default:
                routePreferenceID = RoutePreferenceID.PREFERENCE_RECOMMEND;
                break;
        }

        return routePreferenceID;
    }

    /**
     * 将收藏点信息转换为Poi信息.
     *
     * @param item Favorite，收藏点信息.
     * @return PoiInfoEntity，对应搜索的poi信息.
     */
    public static PoiInfoEntity getPoiInfoByFavorite(Favorite item) {
        if (null == item) {
            return null;
        }

        FavoriteInfo info = new FavoriteInfo()
                .setItemId(item.itemId)
                .setCommonName(item.commonName)
                .setTag(item.tag)
                .setType(item.type)
                .setNewType(item.newType)
                .setCustom_name(item.customName)
                .setClassification(item.classification)
                .setUpdateTime(item.updateTime.getTime())
                .setTop_time(item.topTime);

        PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                .setPid(String.valueOf(item.pid))
                .setAddress(item.address)
                .setName(item.name)
                .setPhone(item.phone)
                .setPoint(new GeoPoint(item.point_x, item.point_y))
                .setFavoriteInfo(info);
        return poiInfoEntity;
    }

    /**
     * 按照固定的策略取整距离数值，与TBT保持一致
     * 1）10公里级别向下取整；
     * 2）1公里级别的四舍五入；
     * 3）1公里以下的暂不修改。
     *
     * @param distance 距离，单位米
     * @return 转换后的距离 距离的Value + 距离的单位
     */
    public static String formatDistance(int distance) {
        if (distance >= 10000) {
            //10公里级
            distance = (distance / 1000) * 1000;
        } else if (distance >= 1000) {
            //1公里级，精确到小数点后一位
            distance = ((distance + 50) / 100) * 100;
        }

        if (distance >= 1000) {
            int kiloMeter = distance / 1000;
            int leftMeter = distance % 1000;
            leftMeter = leftMeter / 100;

            StringBuffer sb = new StringBuffer();

            if (leftMeter > 0) {
                sb.append(kiloMeter);
                sb.append(".");
                sb.append(leftMeter);
            } else {
                sb.append(kiloMeter);
            }
            sb.append("千米");
            return sb.toString();
        } else {
            return distance + "米";
        }
    }

    /**
     * 格式化时间.
     *
     * @return xx小时xx分钟
     */
    public static String formatTime(int second) {
        int minute = second / 60;
        if (minute <= 1) {
            return "1分钟";
        } else if (minute < 60) {
            return minute + "分钟";
        } else {
            StringBuilder builder = new StringBuilder();
            int hour = minute / 60;
            builder.append(hour).append("小时");
            minute = minute % 60;
            if (minute > 0) {
                builder.append(minute).append("分钟");
            }
            return builder.toString();
        }
    }
}
