package com.sgm.navi.hmi.navi;

import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.R;
import com.sgm.navi.service.define.route.RouteWeatherID;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/27
 * Description: [目的在于减少导航Model得代码]
 */
public class NaviGuidanceHelp {

    /***
     * 获取天气图片
     * @param id weatherId
     * @return 图片ID
     */
    public int getWeatherIcon(final RouteWeatherID id) {
        return switch (id) {
            case ROUTE_WEATHER_CLOUDY -> R.drawable.img_weather_cloud;
            case ROUTE_WEATHER_THUNDER -> R.drawable.img_weather_thunder;
            case ROUTE_WEATHER_MORE_CLOUDY -> R.drawable.img_weather_more_cloud;
            case ROUTE_WEATHER_RAIN -> R.drawable.img_weather_rain;
            case ROUTE_WEATHER_SNOW -> R.drawable.img_weather_snow;
            case ROUTE_WEATHER_BIG_RAIN -> R.drawable.img_weather_big_rain;
            case ROUTE_WEATHER_WIND -> R.drawable.img_weather_wind;
            case ROUTE_WEATHER_FOG -> R.drawable.img_weather_fog;
            case ROUTE_WEATHER_HAIL -> R.drawable.img_weather_hail;
            default -> R.drawable.img_weather_sunny;
        };
    }

    /***
     *
     * @param tipManager
     */
    public void mockTestChargeTipMsg(final ChargeTipManager tipManager) {
        ThreadManager.getInstance().postDelay(() -> tipManager.mockTest(), 6000L);
    }
}
