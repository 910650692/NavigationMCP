package com.sgm.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索POi列表点击事件
 * @Date: 2019/1/16 17:03
 */
public interface IQuickSearchItemClickListener {
    /**
     * 搜索POi列表点击事件
     * @param position 点击下标
     * @param name 搜索关键字
     */
    void onItemClick(int position, String name);
}
