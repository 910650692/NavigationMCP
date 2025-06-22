package com.sgm.navi.scene.api.search;


/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索POi列表点击事件
 * @Date: 2019/1/16 17:03
 */
public interface IOnHomeCompanyClickListener {
    /**
     * 编辑框清除按钮点击事件
     */
    void onEditClearClicked();

    /**
     * 设置家/公司类型
     * @param type 类型
     */
    void setHomeCompanyType(int type);
}
