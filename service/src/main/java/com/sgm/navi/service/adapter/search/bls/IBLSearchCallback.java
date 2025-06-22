package com.sgm.navi.service.adapter.search.bls;

/**
 * @author baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 * @version \$Revision1.0\$
 * @param <T> 回调数据泛型
 */
public interface IBLSearchCallback<T> {
    /**
     * 数据请求成功
     * @param taskId 任务ID
     * @param data 请求到的数据
     */
    void onSuccess(int taskId, T data);

    /**
     * 请求失败
     * @param errCode 错误码
     * @param data 请求到的数据
     */
    void onFailure(int errCode, T data);

    /**
     * 当请求数据结束时，无论请求结果是成功，失败或是抛出异常都会执行此方法给用户做处理，通常做网络
     * 请求时可以在此处隐藏“正在加载”的等待控件
     */
    void onComplete();
}
