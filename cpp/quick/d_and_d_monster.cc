#include <string>
#include <vector>
#include <map>

class Monster {
private:
    // Basic attributes
    std::string name;
    std::string type;      // e.g., "Beast", "Dragon", "Undead"
    std::string size;      // e.g., "Tiny", "Small", "Medium", "Large"
    int challengeRating;
    int armorClass;
    int hitPoints;
    int currentHitPoints;
    
    // Ability scores
    struct AbilityScores {
        int strength;
        int dexterity;
        int constitution;
        int intelligence;
        int wisdom;
        int charisma;
    } abilities;
    
    // Combat
    struct Attack {
        std::string name;
        int toHitBonus;
        std::string damage;  // e.g., "2d6+3"
    };
    std::vector<Attack> attacks;
    
    // Special abilities and resistances
    std::vector<std::string> specialAbilities;
    std::vector<std::string> damageResistances;
    std::vector<std::string> damageImmunities;
    std::vector<std::string> conditions;

public:
    // Constructor
    Monster(const std::string& name, const std::string& type, const std::string& size, int cr)
        : name(name), type(type), size(size), challengeRating(cr), currentHitPoints(hitPoints) {
        // Initialize default values
        armorClass = 10;
        hitPoints = 10;
        currentHitPoints = hitPoints;
        
        // Default ability scores (10 in each)
        abilities = {10, 10, 10, 10, 10, 10};
    }
    
    // Getters
    std::string getName() const { return name; }
    std::string getType() const { return type; }
    std::string getSize() const { return size; }
    int getChallengeRating() const { return challengeRating; }
    int getArmorClass() const { return armorClass; }
    int getHitPoints() const { return hitPoints; }
    int getCurrentHitPoints() const { return currentHitPoints; }
    
    // Ability score methods
    int getAbilityScore(const std::string& ability) const {
        if (ability == "STR") return abilities.strength;
        if (ability == "DEX") return abilities.dexterity;
        if (ability == "CON") return abilities.constitution;
        if (ability == "INT") return abilities.intelligence;
        if (ability == "WIS") return abilities.wisdom;
        if (ability == "CHA") return abilities.charisma;
        return 0;
    }
    
    int getAbilityModifier(int score) const {
        return (score - 10) / 2;
    }
    
    // Combat methods
    void takeDamage(int damage) {
        currentHitPoints = std::max(0, currentHitPoints - damage);
    }
    
    void heal(int amount) {
        currentHitPoints = std::min(hitPoints, currentHitPoints + amount);
    }
    
    bool isAlive() const {
        return currentHitPoints > 0;
    }
    
    // Attack management
    void addAttack(const std::string& name, int toHitBonus, const std::string& damage) {
        attacks.push_back({name, toHitBonus, damage});
    }
    
    const std::vector<Attack>& getAttacks() const {
        return attacks;
    }
    
    // Special ability and resistance management
    void addSpecialAbility(const std::string& ability) {
        specialAbilities.push_back(ability);
    }
    
    void addDamageResistance(const std::string& damageType) {
        damageResistances.push_back(damageType);
    }
    
    void addDamageImmunity(const std::string& damageType) {
        damageImmunities.push_back(damageType);
    }
    
    void addCondition(const std::string& condition) {
        conditions.push_back(condition);
    }
    
    bool hasResistance(const std::string& damageType) const {
        return std::find(damageResistances.begin(), damageResistances.end(), damageType) != damageResistances.end();
    }
    
    bool hasImmunity(const std::string& damageType) const {
        return std::find(damageImmunities.begin(), damageImmunities.end(), damageType) != damageImmunities.end();
    }
};



#include <iostream>
#include <iomanip>
// Assume the Monster class is in "monster.hpp"
// #include "monster.hpp"

// Utility function to display monster status
void displayMonsterStatus(const Monster& monster) {
    std::cout << "\n=== " << monster.getName() << " Status ===" << std::endl;
    std::cout << "Type: " << monster.getType() << " | Size: " << monster.getSize() << std::endl;
    std::cout << "CR: " << monster.getChallengeRating() << std::endl;
    std::cout << "HP: " << monster.getCurrentHitPoints() << "/" << monster.getHitPoints() << std::endl;
    std::cout << "AC: " << monster.getArmorClass() << std::endl;
    
    // Display ability scores
    std::cout << "\nAbility Scores:" << std::endl;
    std::cout << "STR: " << std::setw(2) << monster.getAbilityScore("STR") 
              << " (" << std::showpos << monster.getAbilityModifier(monster.getAbilityScore("STR")) << ")" << std::endl;
    std::cout << "DEX: " << std::setw(2) << monster.getAbilityScore("DEX")
              << " (" << monster.getAbilityModifier(monster.getAbilityScore("DEX")) << ")" << std::endl;
    std::cout << "CON: " << std::setw(2) << monster.getAbilityScore("CON")
              << " (" << monster.getAbilityModifier(monster.getAbilityScore("CON")) << ")" << std::endl;
    std::cout << "INT: " << std::setw(2) << monster.getAbilityScore("INT")
              << " (" << monster.getAbilityModifier(monster.getAbilityScore("INT")) << ")" << std::endl;
    std::cout << "WIS: " << std::setw(2) << monster.getAbilityScore("WIS")
              << " (" << monster.getAbilityModifier(monster.getAbilityScore("WIS")) << ")" << std::endl;
    std::cout << "CHA: " << std::setw(2) << monster.getAbilityScore("CHA")
              << " (" << monster.getAbilityModifier(monster.getAbilityScore("CHA")) << ")" << std::endl;
    std::noshowpos;
    
    // Display attacks
    std::cout << "\nAttacks:" << std::endl;
    for (const auto& attack : monster.getAttacks()) {
        std::cout << "- " << attack.name << ": +" << attack.toHitBonus 
                  << " to hit, damage: " << attack.damage << std::endl;
    }
    std::cout << "\nStatus: " << (monster.isAlive() ? "Alive" : "Dead") << std::endl;
    std::cout << "================================\n" << std::endl;
}

// Simulate a combat round
void simulateCombatRound(Monster& monster, int incomingDamage) {
    std::cout << "Combat Round:" << std::endl;
    std::cout << "Monster takes " << incomingDamage << " damage!" << std::endl;
    monster.takeDamage(incomingDamage);
    
    if (monster.isAlive()) {
        std::cout << "Monster is still fighting!" << std::endl;
    } else {
        std::cout << "Monster has been defeated!" << std::endl;
    }
}

int main() {
    // Create a young red dragon
    Monster dragon("Young Red Dragon", "Dragon", "Large", 10);
    
    // Set up the dragon's attacks
    dragon.addAttack("Bite", 10, "2d10+6");
    dragon.addAttack("Claw", 10, "2d6+6");
    dragon.addAttack("Fire Breath", 0, "16d6");
    
    // Add special abilities and resistances
    dragon.addSpecialAbility("Fire Breath (Recharge 5-6)");
    dragon.addDamageImmunity("fire");
    dragon.addSpecialAbility("Legendary Resistance (3/Day)");
    
    // Display initial status
    displayMonsterStatus(dragon);
    
    // Simulate some combat rounds
    std::cout << "Beginning combat simulation...\n" << std::endl;
    
    // Round 1: Dragon takes moderate damage
    simulateCombatRound(dragon, 25);
    displayMonsterStatus(dragon);
    
    // Round 2: Dragon gets healed
    std::cout << "Dragon receives healing magic for 15 HP!" << std::endl;
    dragon.heal(15);
    displayMonsterStatus(dragon);
    
    // Round 3: Dragon takes massive damage
    simulateCombatRound(dragon, 100);
    displayMonsterStatus(dragon);
    
    return 0;
}


