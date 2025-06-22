package com.sgm.navi.scene.callback;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/1
 * Description: [在这里描述文件功能]
 */
public interface OnPowerChangeListener {
    /***
     * 插混或者纯电车电量不足回调
     */
    void onElectricLowerNotify();

    /***
     * 纯油车油量不足回调
     */
    void onGasLowerNotify();
}
