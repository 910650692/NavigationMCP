package com.sgm.navi.mapservice.callback;

public interface OnTurnInfoChangeListener {

    /**
     * 引导TBT信息更新回调.
     *
     * @param turnInfo String，BaseTurnInfo对应的json类型的字符串.
     */
    void onTurnInfoUpdated(String turnInfo);
}
