package com.fy.navi.service.greendao.history;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.greendao.DaoMaster;

import org.greenrobot.greendao.database.Database;
import org.greenrobot.greendao.query.QueryBuilder;

import java.util.Date;
import java.util.List;

/**
 * @Author: fenghong0322
 * @Description: 搜索/导航历史记录数据库管理
 * @CreateDate: 2025-01-07
 */
public class HistoryManager {
    private static final String TAG = HistoryManager.class.getSimpleName();
    private static final String DB_NAME = "history.db";
    private static HistoryManager mManager;
    private HistoryDao mSearchHistoryDao;
    private static final int MAX_USERS = 50; // 假设最大搜索记录数为50

    /**
     * Get instance.
     */
    public static HistoryManager getInstance() {
        if (mManager == null) {
            synchronized (HistoryManager.class) {
                if (mManager == null) {
                    mManager = new HistoryManager();
                }
                return mManager;
            }
        }
        return mManager;
    }

    /**
     * Init database and cloud data.
     */
    public void init() {
        // 数据库对象
        DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(AppContext.mContext, DB_NAME);
        Database db = helper.getWritableDb();
        DaoMaster daoMaster = new DaoMaster(db);
        mSearchHistoryDao = daoMaster.newSession().getHistoryDao();
    }

    /**
     * Use key and value for data history.
     */
    public void insertValue(History info) {
        Logger.d(TAG, "insertOrReplace");
        if (isInsert()) { //  达到限制，不允许插入新数据，只允许更新数据
            insertOrReplace(info);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("无法插入新数据: 数量已达上限！");
        }
    }

    /**
     * 修改或者更新数据
     *
     * @param info
     */
    public void insertOrReplace(History info) {
        History history = GsonUtils.convertToT(info, History.class);
        history.updateTime = new Date();
        mSearchHistoryDao.insertOrReplace(history);
    }

    /**
     * 更新数据
     *
     * @param info
     */
    private void update(History info) {
        History unique = mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.Id.eq(info.id))
                .unique();
        unique.keyWord = info.keyWord;
        unique.poiId = info.poiId;
        unique.startPoint = info.startPoint;
        unique.endPoint = info.endPoint;
        unique.startPoiName = info.startPoiName;
        unique.endPoiName = info.endPoiName;
        unique.type = info.type;
        unique.updateTime = info.updateTime;
        mSearchHistoryDao.update(unique);
    }

    /**
     * 删除某个数据
     *
     * @param id
     */
    public void deleteValue(long id) {
        mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.Id.eq(id))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 清空全部数据
     */
    public void deleteAll() {
        mSearchHistoryDao.deleteAll();
    }

    /**
     * 通过key查找其对应info
     *
     * @param id
     * @return
     */
    public History getValueByKey(int id) {
        return mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.Id.eq(id))
                .unique();
    }

    /**
     * 通过数据type查找其对应info
     *
     * @param keyWord
     * @return
     */
    public  List<History> getValueByType(String keyWord) {
        return mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.KeyWord.eq(keyWord))
                .list();
    }

    /**
     * 获取未完成的导航
     */
    public History getUncompletedNavi() {
        // 如果查询结果不唯一，返回第一条数据
        List<History> histories = mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.IsCompleted.eq(false))
                .list();
        if (!ConvertUtils.isEmpty(histories)) {
            return histories.get(0);
        } else {
            return null; //没有找到数据
        }
    }

    /**
     * 获取全部(原始)数据
     *
     * @return all data list,String
     */
    public List<History> loadAll() {
        return mSearchHistoryDao.loadAll();
    }

    /**
     * 检查当前搜索历史数是否已达到限制
     *
     * @return
     */
    public boolean isInsert() {
        long currentSearchCount = mSearchHistoryDao.count();
        if (currentSearchCount >= MAX_USERS) {
            // 达到限制，不允许插入新数据
            Logger.e(TAG, "Cannot insert history: Maximum history limit reached.");
            return false;
        }
        // 可插入新搜索记录
        return true;
    }

    /**
     * 分页查询搜索历史数据
     *
     * @param pageNum  页码（从1开始）
     * @param pageSize 每页显示的记录数
     * @return 搜索历史列表
     */
    public List<History> loadHistoryByPage(int pageNum, int pageSize) {
        // 计算偏移量（注意：页码通常从1开始，但SQL中的LIMIT偏移量从0开始，因此需要减1）
        int offset = (pageNum - 1) * pageSize;
        // 使用QueryBuilder构建查询
        QueryBuilder<History> queryBuilder = mSearchHistoryDao.queryBuilder();
        // 按 UpdateTime 降序排列. orderDesc 表示降序
        queryBuilder.orderDesc(HistoryDao.Properties.UpdateTime);
        // 设置LIMIT和OFFSET
        queryBuilder.limit(pageSize).offset(offset);
        // 执行查询并返回结果
        List<History> historyList = queryBuilder.list();
        return historyList;
    }
}
