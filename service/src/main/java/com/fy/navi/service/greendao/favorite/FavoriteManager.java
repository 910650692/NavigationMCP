package com.fy.navi.service.greendao.favorite;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.greendao.DaoMaster;

import org.greenrobot.greendao.database.Database;

import java.util.List;


public class FavoriteManager {
    private static final String TAG = FavoriteManager.class.getSimpleName();
    private static final String DB_NAME = "favorite.db";
    private static FavoriteManager mManager;
    private FavoriteDao mFavoriteDao;

    /**
     * Get instance.
     * @return instance
     */
    public static FavoriteManager getInstance() {
        if (mManager == null) {
            synchronized (FavoriteManager.class) {
                if (mManager == null) {
                    mManager = new FavoriteManager();
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
        mFavoriteDao = daoMaster.newSession().getFavoriteDao();
    }

    /**
     * Use key and value for data favorite.
     * @param info 收藏点信息
     */
    public void insertValue(final Favorite info) {
        Logger.d(TAG, "insertOrReplace");
        insertOrReplace(info);
    }

    /**
     * 修改或者更新数据
     * @param info 收藏点信息
     */
    public void insertOrReplace(final Favorite info) {
        final Favorite favorite = GsonUtils.convertToT(info, Favorite.class);
        mFavoriteDao.insertOrReplace(favorite);
    }

    /**
     * 更新数据
     * @param itemId  收藏点唯一码
     * @param customName  自定义名称 重命名时编辑的字段
     */
    public void updateCustomName(final String itemId, final String customName) {
        final Favorite unique = mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MItemId.eq(itemId))
                .unique();
        if (unique == null) {
            return;
        }
        unique.setMCustomName(customName);
        mFavoriteDao.update(unique);
    }

    /**
     * 更新数据
     * @param itemId  收藏点唯一码
     * @param topTime  置顶时间
     */
    public void updateTopTime(final String itemId, final long topTime) {
        final Favorite unique = mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MItemId.eq(itemId))
                .unique();
        if (unique == null) {
            return;
        }
        unique.setMTopTime(topTime);
        mFavoriteDao.update(unique);
    }

    /**
     * 删除某个数据
     *
     * @param itemId  收藏点唯一码
     */
    public void deleteValue(final String itemId) {
        mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MItemId.eq(itemId))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 清空全部数据
     */
    public void deleteAll() {
        mFavoriteDao.deleteAll();
    }

    /**
     * 通过itemId查找其对应info
     * @param itemId 收藏点唯一码
     * @return Favorite
     */
    public Favorite getValueByKey(final String itemId) {
        return mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MItemId.eq(itemId))
                .unique();
    }

    /**
     * 查找 itemId 对应的本地数据是否为收藏点
     * @param itemId 收藏点唯一码
     * @return true 已收藏，false 未收藏
     */
    public boolean isFavorite(final String itemId) {
        final Favorite favorite = mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MItemId.eq(itemId))
                .unique();
        return favorite != null;
    }

    /**
     * 通过commonName查找其对应 list
     * @param commonName 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return Favorite
     */
    public List<Favorite> getValueByCommonName(final int commonName) {
        final List<Favorite> saveData = mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MCommonName.eq(commonName))
                .list();
        return saveData;
    }

    /**
     * 获取未置顶列表
     * @return 未置顶列表
     */
    public List<Favorite> getFavoriteNotTop() {
        return mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MCommonName.eq(0),
                        FavoriteDao.Properties.MTopTime.eq(0))
                .list();
    }

    /**
     * 获取已置顶列表
     * @return 已置顶列表
     */
    public List<Favorite> getValueByTopTime() {
        return mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MCommonName.eq(0),
                        FavoriteDao.Properties.MTopTime.notEq(0))
                .list();
    }

    /**
     * 本地删除指定的类型全部数据
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void deleteByFavoriteType(final int favoriteType){
        mFavoriteDao.queryBuilder()
                .where(FavoriteDao.Properties.MCommonName.eq(favoriteType))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 获取全部(原始)数据
     * @return all data list,String
     */
    public List<Favorite> loadAll() {
        return mFavoriteDao.loadAll();
    }


}
