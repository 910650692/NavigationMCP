package com.sgm.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface ISceneSearchHistory {

    /**
     * 获取历史关键字搜索记录列表
     */
    void getSearchKeywordRecord();

    /**
     * 清除历史关键字搜索记录列表
     */
    void clearSearchKeywordRecord();

}
