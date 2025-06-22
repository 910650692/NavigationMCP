package com.sgm.navi.hmi.message;

import android.text.TextUtils;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.message.MessageCenterInfo;
import com.sgm.navi.service.define.message.MessageCenterType;
import com.sgm.navi.service.greendao.CommonManager;
public class MessageCenterHelper {

    private CommonManager mCommonManager = null;
    public MessageCenterHelper() {
        this.mCommonManager = CommonManager.getInstance();
        this.mCommonManager.init();
    }

    /**
     * 管理消息
     * @return 数据
     * @param messageCenterType  类型
     */
    public MessageCenterInfo manageMessage(final MessageCenterType messageCenterType) {
//        if(messageCenterType == MessageCenterType.ROAD_LIMIT){
//           return new MessageCenterInfo(messageCenterType,
//                   ResourceUtils.Companion.getInstance().getString(R.string.message_center_check),0,
//                   ResourceUtils.Companion.getInstance().getString(R.string.message_center_limit),
//                    "",new Date(),0);
//        }else if(messageCenterType == MessageCenterType.MAP_UPDATE_15){
//            return new MessageCenterInfo(messageCenterType,
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_update),0,
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_need_update),
//                    "",new Date(),0);
//        }else if(messageCenterType == MessageCenterType.MAP_UPDATE_45){
//            return new MessageCenterInfo(messageCenterType,
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_update),0,
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_netless),
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_recommend_update),
//                    new Date(),0);
//        }else if(messageCenterType == MessageCenterType.WEATHER){
//            return new MessageCenterInfo(messageCenterType,
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_update),0,
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_netless),
//                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_recommend_update),
//                    new Date(),0);
//        }else if(messageCenterType == MessageCenterType.PHONE_MESSAGE){
//        }
        return null;
    }

    /**
     * 判断是否大于15天
     * @return 是否15天
     * @param lastConnectedTime  最后联网时间
     */
    public  boolean isNotConnectedFor15Days(final long lastConnectedTime) {
        if (lastConnectedTime != -1) {
            final long currentTime = System.currentTimeMillis();
            final long timeDifference = currentTime - lastConnectedTime;
            final long m15DayNetLess = 15L * 24 * 60 * 60 * 1000;
            return timeDifference >= m15DayNetLess;
        }
        return false;
    }

    /**
     * 存入无网络的时间  45天
     */
    public void saveLast45ConnectedTime() {
        mCommonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_NET_LESS,String.valueOf(System.currentTimeMillis()));
    }

    /**
     * 获取无网络的时间  45天
     * @return 返回最后时间
     */
    private long getLast45ConnectedTime() {
        final String value = mCommonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_NET_LESS);
        if(!TextUtils.isEmpty(value)){
            return Long.parseLong(value);
        }
        return -1;
    }

    /**
     * 判断是否大于45天
     * @return 是否45天
     */
    public  boolean isNotConnectedFor45Days() {
        final long lastConnectedTime = getLast45ConnectedTime();
        if (lastConnectedTime != -1) {
            final long currentTime = System.currentTimeMillis();
            final long timeDifference = currentTime - lastConnectedTime;
            final long m45DayNetLess = 45L * 24 * 60 * 60 * 1000;
            return timeDifference >= m45DayNetLess;
        }
        return false;
    }
}
