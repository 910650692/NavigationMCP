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

public class HistoryManager {
    private static final String TAG = HistoryManager.class.getSimpleName();
    private static final String DB_NAME = "history.db";
    private static HistoryManager mManager;
    private HistoryDao mSearchHistoryDao;
    private static final int MAX_USERS = 50; // 假设最大搜索记录数为50

    /**
     * Get instance.
     * @return instance
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
        final DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(AppContext.getInstance().getMContext(), DB_NAME);
        final Database db = helper.getWritableDb();
        final DaoMaster daoMaster = new DaoMaster(db);
        mSearchHistoryDao = daoMaster.newSession().getHistoryDao();
    }

    /**
     * Use key and value for data history.
     * @param info save info
     */
    public void insertValue(final History info) {
        Logger.d(TAG, "insertOrReplace");
        if (isInsert()) { //  达到限制，不允许插入新数据，只允许更新数据
            insertOrReplace(info);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("无法插入新数据: 数量已达上限！");
        }
    }

    /**
     * 修改或者更新数据
     * @param info save info
     */
    public void insertOrReplace(final History info) {
        final History history = GsonUtils.convertToT(info, History.class);
        history.setMUpdateTime(new Date());
        mSearchHistoryDao.insertOrReplace(history);
    }

    /**
     * 更新数据
     * @param info save info
     */
    private void update(final History info) {
        final History unique = mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MId.eq(info.getMId()))
                .unique();
        unique.setMKeyWord(info.getMKeyWord());
        unique.setMPoiId(info.getMPoiId());
        unique.setMStartPoint(info.getMStartPoint());
        unique.setMEndPoint(info.getMEndPoint());
        unique.setMStartPoiName(info.getMStartPoiName());
        unique.setMEndPoiName(info.getMEndPoiName());
        unique.setMType(info.getMType());
        unique.setMUpdateTime(info.getMUpdateTime());
        mSearchHistoryDao.update(unique);
    }

    /**
     * 删除某个数据
     * @param id 唯一值
     */
    public void deleteValue(final long id) {
        mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MId.eq(id))
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
     * @param id 唯一值
     * @return History
     */
    public History getValueByKey(final int id) {
        return mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MId.eq(id))
                .unique();
    }

    /**
     * 通过数据type查找其对应info
     * @param keyWord 数据type
     * @return History
     */
    public  List<History> getValueByType(final String keyWord) {
        return mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MKeyWord.eq(keyWord))
                .list();
    }

    /**
     * 通过数据type查找其对应info
     * @param type 数据type
     * @param poiId 数据id
     * @return History
     */
    public boolean isDataExist(final int type, final String poiId) {
        final List<History> histories;

        histories = mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MRideRunType.eq(type),
                        HistoryDao.Properties.MPoiId.eq(poiId))
                .list();
        return !histories.isEmpty();
    }


    /**
     * 通过数据type查找其对应info
     * @param type 数据type
     * @return History
     */
    public List<History> getValueByType(final int type) {
        return mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MType.eq(type))
                .list();
    }

    /**
     * 通过数据type删除其对应info
     * @param type 数据type
     */
    public void deleteValueByKey(final int type) {
        mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MType.eq(type))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 通过数据type删除其对应info
     * @param fileName 数据文件名
     */
    public void deleteValueByFileName(final String fileName) {
        mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MTrackFileName.eq(fileName))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }


    /**
     * 获取未完成的导航
     * @return History
     */
    public History getUncompletedNavi() {
        // 如果查询结果不唯一，返回第一条数据
        final List<History> histories = mSearchHistoryDao.queryBuilder()
                .where(HistoryDao.Properties.MIsCompleted.eq(false))
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
     * @return boolean
     */
    public boolean isInsert() {
        final long currentSearchCount = mSearchHistoryDao.count();
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
    public List<History> loadHistoryByPage(final int pageNum, final int pageSize) {
        // 计算偏移量（注意：页码通常从1开始，但SQL中的LIMIT偏移量从0开始，因此需要减1）
        final int offset = (pageNum - 1) * pageSize;
        // 使用QueryBuilder构建查询
        final QueryBuilder<History> queryBuilder = mSearchHistoryDao.queryBuilder();
        // 按 UpdateTime 降序排列. orderDesc 表示降序
        queryBuilder.orderDesc(HistoryDao.Properties.MUpdateTime);
        // 设置LIMIT和OFFSET
        queryBuilder.limit(pageSize).offset(offset);
        // 执行查询并返回结果
        final List<History> historyList = queryBuilder.list();
        return historyList;
    }
}
